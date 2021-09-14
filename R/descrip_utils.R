# ------------------------------------------------------------------------------
# General helper functions
# ------------------------------------------------------------------------------
#' Check if an object is a "Date" object
#' 
#' @param x the variable
#' 
#' @return A logical flag indicating whether the variable is a "Date" variable.
#' 
#' @keywords internal
#' @noRd
is.Date <- function(x) {
    inherits(x, "Date")
}

# ------------------------------------------------------------------------------
# Survival-object-specific helper functions
# ------------------------------------------------------------------------------

#' Compute Kaplan-Meier estimates of the survival curve
#' 
#' @param x the data
#' 
#' @return Kaplan-Meier estimates of the survival curve
#' 
#' @keywords internal
#' @noRd
KM <- function(x) {
    if (!survival::is.Surv(x)) 
        stop("x must be a Surv object")
    x <- x[!is.na(x)]
    # time of observations
    obs <- x[, 1]
    # event status (yes/no, 1/0)
    ev <- x[, 2]
    # censored observation (yes/no, 1/0)
    ce <- 1 - ev
    if (length(obs) == 0) 
        stop("No data to estimate survival curve")
    N <- length(obs)
    # if there are no observations at time 0, add in values for time zero (no events)
    if (!any(obs == 0)) {
        obs <- c(0, obs)
        ev <- c(0, ev)
        ce <- c(0, ce) 
    }
    # reorder vectors according to observation time
    i <- order(obs, 1 - ev)
    obs <- obs[i]
    ev <- ev[i]
    ce <- ce[i]

    ev <- rev(cumsum(rev(ev)))
    ce <- rev(cumsum(rev(ce)))
    # cumulative number of people at risk for event
    n <- ce + ev
    # remove duplicate observations
    i <- !duplicated(obs)
    obs <- obs[i]
    n <- n[i]
    ev <- ev[i]
    ev <- ev - c(ev[-1], 0)
    ce <- ce[i]
    ce <- ce - c(ce[-1], 0)
    v <- N * cumsum(ev/(n - ev)/n)
    S <- exp(cumsum(log(1 - ev/n)))
    if (is.na(S[length(S)])) 
        S[length(S)] <- 0
    rslt <- data.frame(t = obs, atrisk = n, events = ev, 
                       censored = ce, S = S, v = v)
    class(rslt) <- c("KM", "data.frame")
    rslt
}

#' Compute a sum over a Kaplan-Meier curve
#' 
#' @param x an object of class \code{KM} (from a call to \code{KM})
#' @param times a vector of times to use in computation
#' @param rightCtsCDF logical, is the cdf right-continuous? (defaults to \code{TRUE})
#' 
#' @return the summed survival curve, over the entered times
#' 
#' @keywords internal
#' @noRd
sum_KM <- function(x, times, rightCtsCDF = TRUE) {
    if (!inherits(x, "KM")) 
        stop("x must be a KM object")
    if (rightCtsCDF) {
        rslt <- as.vector(apply(matrix(rep(times, each = length(x$t)), 
                                       length(x$t)) >= x$t, 2, sum)) + 1
    }
    else rslt <- as.vector(apply(matrix(rep(times, each = length(x$t)), 
                                        length(x$t)) > x$t, 2, sum)) + 1
    if (x$S[length(x$S)] > 0) 
        rslt[times > x$t[length(x$t)]] <- NA
    c(1, x$S)[rslt]
}

#' Compute quantiles of the Kaplan-Meier curve
#' 
#' @inheritParams sum_KM
#' @param probs a vector of proportions of interest for the quantiles
#' 
#' @return quantiles of the estimated Kaplan-Meier curve
#' 
#' @keywords internal
#' @noRd
quantile_KM <- function(x, probs) {
    rslt <- length(probs)
    for (i in 1:length(probs)) {
        p <- 1 - probs[i]
        j <- abs(x$S - p) < 1e-15 & x$events > 0
        if (any(j)) {
            if (abs(p - min(x$S)) < 1e-15) {
                rslt[i] <- (x$t[j] + max(x$t))/2
            }
            else {
                rslt[i] <- (x$t[j] + min(x$t[x$t > x$t[j] & x$events > 0]))/2
            }
        }
        else {
            j <- sum(x$S > p)
            if (j == length(x$S) | p == 1) {
                rslt[i] <- NA
            }
            else rslt[i] <- x$t[j + 1]
        }
    }
    rslt
}

#' Compute the restricted mean survival time
#' 
#' @inheritParams sum_KM
#' @param restriction a value used for computing restricted means, standard deviations, 
#'   and geometric means with censored time-to-event data. The default value of 
#'   \code{Inf} will cause restrictions at the highest observation. Note that the 
#'   same value is used for all variables of class \code{Surv}.
#' 
#' @return the restricted mean survival time
#' @keywords internal
#' @noRd
mean_KM <- function(x, restriction = Inf) {
    if (length(restriction) == 1) 
        restriction <- c(x$t[1] - 1, restriction)
    if (restriction[2] == Inf) 
        restriction[2] <- x$t[length(x$t)]
    tms <- c(restriction[1], x$t[x$t > restriction[1] & x$t < restriction[2]], 
             restriction[2])
    s <- sum_KM(x, restriction)
    s <- c(s[1], x$S[x$t > restriction[1] & x$t < restriction[2]], s[2])
    ne <- tms <= 0
    po <- tms >= 0
    neS <- 1 - s[ne]
    neX <- abs(c(diff(tms[ne]), 0))
    neI <- neS != 0 & neX != 0
    if (sum(neI) > 0) 
        rslt <- -sum(neS[neI] * neX[neI])
    else rslt <- 0
    poS <- s[po]
    poX <- c(diff(tms[po]), 0)
    poI <- poS != 0 & poX != 0
    if (sum(poI) > 0) 
        rslt <- rslt + sum(poS[poI] * poX[poI])
    attr(rslt, "restriction") <- restriction
    rslt
}

# ------------------------------------------------------------------------------
# Compute descriptive statistics for different object classes
# ------------------------------------------------------------------------------

#' Descriptive Statistics for a Vector of Data
#' @param x a vector of data
#' @param probs a vector of proportions to use in describing (e.g., 0.25, 0.5)
#' @param thresholds a vector of thresholds used to dichotomize variables.
#' @param geometricMean logical, should we compute the geomtric mean? Defaults
#'   to \code{FALSE}. 
#' @param geomInclude if not \code{FALSE}, includes the geometric mean in the 
#'   descriptive statistics. Default is \code{FALSE}.
#' @param replaceZeroes if not \code{FALSE}, this indicates a value to be used 
#'   in place of zeroes when computing a geometric mean. If \code{TRUE}, a value 
#'   equal to one-half the lowest nonzero value is used. If a numeric value is 
#'   supplied, that value is used for all variables.
#' 
#' @return A matrix of descriptive statistics
#'   
#' @keywords internal
#' @noRd
describe_vector <- function(x, probs = c(0.25, 0.5, 0.75), thresholds = NULL, 
                     geometricMean = FALSE, geomInclude = FALSE, 
                     replaceZeroes = FALSE) {
    if (is.character(x)) {
        x <- as.factor(x)
    }
    if (is.factor(x) | is.logical(x)) {
        x <- as.numeric(x)
    }
    if (!geomInclude) {
        geometricMean <- geomInclude
    }
    ntholds <- ifelse(is.null(thresholds), 0, dim(thresholds)[1])
    probs <- sort(unique(c(probs, 0, 1)))
    # add sample size
    rslt <- length(x)
    if (rslt == 0) {
        rslt <- c(rslt, rep(NaN, 7 + length(probs) + ntholds))
    }
    else {
        # add number missing
        u <- is.na(x)
        rslt <- c(rslt, sum(u))
        # only look at non-missing observations for remaining summaries
        x <- x[!u]
        if (length(x) == 0 | is.character(x)) {
            if (!geomInclude) {
                rslt <- c(rslt, rep(NA, 6 + length(probs) + ntholds))
            } else {
                rslt <- c(rslt, rep(NA, 7 + length(probs) + ntholds))
            }
        } else {
            if (geomInclude) {
                # add mean, sd, geometric mean, quantiles
                rslt <- c(rslt, mean(x), stats::sd(x), ifelse1(
                    geometricMean, 
                    exp(mean(log(ifelse(x == 0, replaceZeroes, x)))), 
                    NA
                ), stats::quantile(x, probs))
            } else {
                # add mean, sd, quantiles
                rslt <- c(rslt, mean(x), stats::sd(x), stats::quantile(x, probs))
            }
            if (ntholds > 0) {
                # add thresholds if any are specified
                for (j in 1:ntholds) {
                    u <- ifelse1(
                        thresholds[j, 1] == 0, x > thresholds[j, 2], x >= thresholds[j, 2]
                    ) & ifelse1(
                        thresholds[j, 3] == 0, x < thresholds[j, 4], x <= thresholds[j, 4]
                    )
                    rslt <- c(rslt, mean(u))
                }
            }
            # add in firstEvent and lastEvent
            rslt <- c(rslt, Inf, NA, NA)
        }
    }
    if (length(x) > 0) {
        # add in a 0 if the vector is not a date
        rslt <- matrix(c(rslt, 0), 1)
    }
    else {
        rslt <- matrix(rslt, 1)
    }
    # set quantile names
    qnames <- paste(format(100 * probs), "%", sep = "")
    qnames[probs == 0.5] <- " Mdn"
    qnames[probs == 0] <- " Min"
    qnames[probs == 1] <- " Max"
    tnames <- NULL
    # set threshold column names
    if (ntholds > 0) {
        tholds <- thresholds
        tholds[tholds == Inf | tholds == -Inf] <- 0
        tnames <- paste(
            sep = "", "Pr", ifelse(
                thresholds[, 2] == -Inf, 
                paste(sep = "", ifelse(thresholds[, 3] == 0, "<", "<="), format(tholds[, 4])), 
                ifelse(
                    thresholds[, 4] == Inf, 
                    paste(sep = "", ifelse(thresholds[, 1] == 0, ">", ">="), format(tholds[, 2])), 
                    paste(
                        sep = "", ifelse(thresholds[, 1] == 0, "(", "["), 
                        format(tholds[, 2]), ",", format(tholds[, 4]), 
                        ifelse(thresholds[, 3] == 0, ")", "]")
                    )
                )
            )
        )
    }
    if (geomInclude) {
        dimnames(rslt) <- list("", c("N", "Msng", "Mean", 
                                     "Std Dev", "Geom Mn", qnames, tnames, "restriction", 
                                     "firstEvent", "lastEvent", "isDate"))
    }
    else {
        dimnames(rslt) <- list("", c("N", "Msng", "Mean", 
                                     "Std Dev", qnames, tnames, "restriction", "firstEvent", 
                                     "lastEvent", "isDate"))
    }
    rslt
}
 
#' Describe a Survival variable
#' 
#' @param x the variable of interest
#' @param resctriction a value used for computing restricted means, 
#'   standard deviations, and geometric means with censored time to event data. 
#'   The default value of \code{Inf} will cause restrictions at the highest 
#'   observation. Note that the same value is used for all variables of class \code{Surv}.
#' @inheritParams describe_vector
#' 
#' @return A matrix with descriptive statistics for the Surv object
#' @keywords internal
#' @noRd
describe_surv <- function(x, probs = c(0.25, 0.5, 0.75), thresholds = NULL, 
                     geometricMean = FALSE, geomInclude = FALSE, 
                     replaceZeroes = FALSE, restriction = Inf) {
    if (!survival::is.Surv(x)) 
        stop("x must be a Surv object")
    ntholds <- if (is.null(thresholds)) 
        0
    else dim(thresholds)[1]
    probs <- sort(unique(c(probs, 0, 1)))
    rslt <- dim(x)[1]
    if (rslt == 0) {
        rslt <- c(rslt, rep(NaN, 7 + length(probs) + ntholds))
    }
    else {
        u <- is.na(x)
        rslt <- c(rslt, sum(u))
        x <- x[!u]
        if (dim(x)[1] == 0) {
            rslt <- c(rslt, rep(NA, 6 + length(probs) + ntholds))
        }
        else {
            z <- KM(x)
            tmp1 <- mean_KM(z, restriction)
            x2 <- x
            x2[, 1] <- x2[, 1]^2
            z2 <- KM(x2)
            # compute standard deviation as \sqrt{E[X^2] - E[X]^2}
            tmp2 <- sqrt(mean_KM(z2, restriction^2) - tmp1^2)
            if (geometricMean) {
                x2 <- x
                x2[, 1] <- ifelse(x2[, 1] == 0, log(replaceZeroes), 
                                  log(x2[, 1]))
                z2 <- KM(x2)
                tmp3 <- exp(mean_KM(z2, log(restriction)))
            }
            else tmp3 <- NA
            if (any(x[, 2] == 1)) {
                firstEvent <- min(x[x[, 2] == 1, 1])
                lastEvent <- max(x[x[, 2] == 1, 1])
            }
            else {
                firstEvent <- Inf
                lastEvent <- -Inf
            }
            if (geomInclude) {
                rslt <- c(rslt, tmp1, tmp2, tmp3, min(x[, 1]), 
                          quantile_KM(z, probs[-c(1, length(probs))]), max(x[, 
                                                                     1]))
            }
            else {
                rslt <- c(rslt, tmp1, tmp2, min(x[, 1]), quantile_KM(z, 
                                                             probs[-c(1, length(probs))]), max(x[, 1]))
            }
            if (ntholds > 0) {
                for (j in 1:ntholds) {
                    rslt <- c(rslt, ifelse1(thresholds[j, 1] == 
                                                0, sum_KM(z, thresholds[j, 2]), sum_KM(z, thresholds[j, 
                                                                                               2], FALSE)) - ifelse1(thresholds[j, 4] == Inf, 
                                                                                                                 0, ifelse1(thresholds[j, 3] == 0, sum_KM(z, 
                                                                                                                                                       thresholds[j, 4], FALSE), sum_KM(z, thresholds[j, 
                                                                                                                                                                                               4]))))
                }
            }
            rslt <- c(rslt, attr(tmp1, "restriction")[2], 
                      firstEvent, lastEvent)
        }
    }
    rslt <- matrix(c(rslt, 0), 1)
    qnames <- paste(format(100 * probs), "%", sep = "")
    qnames[probs == 0.5] <- " Mdn"
    qnames[probs == 0] <- " Min"
    qnames[probs == 1] <- " Max"
    tnames <- NULL
    if (ntholds > 0) {
        tholds <- thresholds
        tholds[tholds == Inf | tholds == -Inf] <- 0
        tnames <- paste(sep = "", "Pr", ifelse(thresholds[, 
                                                          2] == -Inf, paste(sep = "", ifelse(thresholds[, 
                                                                                                        3] == 0, "<", "<="), format(tholds[, 4])), ifelse(thresholds[, 
                                                                                                                                                                     4] == Inf, paste(sep = "", ifelse(thresholds[, 
                                                                                                                                                                                                                  1] == 0, ">", ">="), format(tholds[, 2])), paste(sep = "", 
                                                                                                                                                                                                                                                                   ifelse(thresholds[, 1] == 0, "(", "["), format(tholds[, 
                                                                                                                                                                                                                                                                                                                         2]), ",", format(tholds[, 4]), ifelse(thresholds[, 
                                                                                                                                                                                                                                                                                                                                                                          3] == 0, ")", "]")))))
    }
    if (geomInclude) {
        dimnames(rslt) <- list("", c("N", "Msng", "Mean", 
                                     "Std Dev", "Geom Mn", qnames, tnames, "restriction", 
                                     "firstEvent", "lastEvent", "isDate"))
    }
    else {
        dimnames(rslt) <- list("", c("N", "Msng", "Mean", 
                                     "Std Dev", qnames, tnames, "restriction", "firstEvent", 
                                     "lastEvent", "isDate"))
    }
    rslt
}

#' Describe a vector variable within strata
#' 
#' @param x the vector of data to describe
#' @param strata the strata
#' @param subset a vector indicating a subset within which descriptive statistics
#'   are desired.
#' @param probs a vector of proportions to use for quantiles
#' @param thresholds a vector of thresholds used to dichotomize variables.
#' @param geomInclude if not \code{FALSE} (the default), includes the geometric mean 
#'   in the descriptive statistics.
#' @param replaceZeroes if not \code{FALSE} (the default), this indicates a value 
#'   to be used in place of zeroes when computing a geometric mean. If \code{TRUE}, 
#'   a value equal to one-half the lowest nonzero value is used. If a numeric value 
#'   is supplied, that value is used for all variables.
#' 
#' @return the descriptive statistics within strata
#' 
#' @keywords internal
#' @noRd
describe_stratified_vector <- function(x, strata, subset, 
                                       probs = c(0.25, 0.5, 0.75), 
                                       thresholds = NULL, geomInclude = FALSE, 
                                       replaceZeroes = FALSE) {
    if (is.null(subset)) 
        subset <- rep(TRUE, length(x))
    if (length(x) != length(subset)) 
        stop("length of variables must match length of subset")
    if (is.null(strata)) 
        strata <- rep(1, length(x))
    if (length(x) != length(strata)) 
        stop("length of variables must match length of strata")
    x <- x[subset]
    if (is.factor(x) | all(x[!is.na(x)] %in% c(0, 1))) {
        geometricMean <- FALSE
    } else {
        geometricMean <- !any(x[!is.na(x)] < 0)
    }
    if (is.logical(replaceZeroes)) {
        if (!replaceZeroes | is.factor(x) | all(x[!is.na(x)] %in% 
                                                c(0, 1))) {
            replaceZeroes <- NA
        } else {
            replaceZeroes <- min(x[!is.na(x) & x > 0])/2
        }
    }
    strata <- strata[subset]
    s <- sort(unique(strata))
    rslt <- describe_vector(x, probs, thresholds, geometricMean, 
                     geomInclude, replaceZeroes)
    if (length(s) > 1) {
        for (i in s) rslt <- rbind(rslt, describe_vector(x[strata == 
                                                        i & !is.na(strata)], probs, thresholds, geometricMean, 
                                                  geomInclude, replaceZeroes))
        if (any(is.na(strata))) {
            rslt <- rbind(rslt, describe_vector(x[is.na(strata)], 
                                         probs, thresholds, geometricMean, geomInclude, 
                                         replaceZeroes))
            dimnames(rslt)[[1]] <- format(c("All", paste("  Str", 
                                                         format(c(format(s), "NA")))))
        }
        else dimnames(rslt)[[1]] <- format(c("All", paste("  Str", 
                                                          format(s))))
    }
    rslt
}

#' Describe a Date object within strata
#' 
#' @inheritParams describe_stratified_vector
#' 
#' @return descriptive statistics within strata for a Date object
#' 
#' @keywords internal
#' @noRd
describe_stratified_date <- function(x, strata, subset, 
                                     probs = c(0.25, 0.5, 0.75), 
                                     thresholds = NULL, geomInclude = FALSE, 
                                     replaceZeroes = FALSE) {
    if (!is.Date(x)) 
        stop("x must be a Date object")
    xi <- as.integer(x)
    rslt <- describe_stratified_vector(xi, strata, subset, probs, thresholds, 
                      geomInclude, replaceZeroes)
    rslt[, "isDate"] <- 1
    rslt
}

#' Describe a stratified Surv object
#' 
#' @inheritParams describe_stratified_vector
#' @inheritParams describe_surv
#' 
#' @return descriptive statistics within strata for a Surv object
#'
#' @keywords internal
#' @noRd
describe_stratified_surv <- function(x, strata, subset, 
                                     probs = c(0.25, 0.5, 0.75), 
                                     thresholds = NULL, geomInclude = FALSE, 
                                     replaceZeroes = FALSE, restriction = Inf) {
    if (!survival::is.Surv(x)) 
        stop("x must be a Surv object")
    n <- dim(x)[1]
    if (is.null(subset)) 
        subset <- rep(TRUE, n)
    if (n != length(subset)) 
        stop("length of variables must match length of subset")
    if (is.null(strata)) 
        strata <- rep(1, n)
    if (n != length(strata)) 
        stop("length of variables must match length of strata")
    x <- x[subset]
    if (geomInclude) {
        geometricMean <- !any(x[!is.na(x), 1] < 0)
    }
    else {
        geometricMean <- FALSE
    }
    if (is.logical(replaceZeroes)) {
        if (replaceZeroes) {
            replaceZeroes <- min(x[!is.na(x) & x[, 1] > 0, 1])/2   
        } else {
            replaceZeroes <- NA
        }
    }
    strata <- strata[subset]
    s <- sort(unique(strata))
    rslt <- describe_surv(x, probs, thresholds, geomInclude, geometricMean, 
                          replaceZeroes, restriction)
    if (length(s) > 1) {
        for (i in s) {
            rslt <- rbind(rslt, describe_surv(x[strata == i & !is.na(strata)], 
                                              probs, thresholds, geomInclude, 
                                              geometricMean, replaceZeroes, restriction))
        }
        if (any(is.na(strata))) {
            rslt <- rbind(rslt, describe_surv(x[is.na(strata)], probs, 
                                              thresholds, geomInclude, geometricMean, 
                                              replaceZeroes, restriction))
            dimnames(rslt)[[1]] <- format(c("All", 
                                            paste("  Str", format(c(format(s), "NA")))))
        } else {
            dimnames(rslt)[[1]] <- format(c("All", paste("  Str", format(s))))
        }
    }
    rslt
}

#' Describe a matrix object within strata
#' 
#' @inheritParams describe_stratified_vector
#' 
#' @return descriptive statistics within strata for a matrix object
#' 
#' @keywords internal
#' @noRd
describe_stratified_matrix <- function(x, strata, subset, 
                                       probs = c(0.25, 0.5, 0.75), 
                                       thresholds = NULL, geomInclude = FALSE, 
                                       replaceZeroes = FALSE) {
    if (!is.matrix(x)) {
        stop("x must be a matrix")   
    }
    p <- dim(x)[2]
    nms <- dimnames(x)[[2]]
    if (is.null(nms)) {
        nms <- paste("V", 1:p, sep = "")   
    }
    rslt <- NULL
    for (i in 1:p) {
        rslt <- rbind(
            rslt, 
            describe_stratified_vector(x[, i], strata, subset, probs, 
                                       thresholds, geomInclude, replaceZeroes)
        )
    }
    dimnames(rslt)[[1]] <- paste(format(rep(nms, each = (dim(rslt)[1])/p)), dimnames(rslt)[[1]])
    rslt
}

#' Describe a list object within strata
#' 
#' @inheritParams describe_stratified_surv
#' 
#' @return descriptive statistics for each element of the list within strata
#' 
#' @keywords internal
#' @noRd
describe_stratified_list <- function(x, strata, subset, 
                                     probs = c(0.25, 0.5, 0.75), 
                                     thresholds = NULL, geomInclude = FALSE, 
                                     replaceZeroes = FALSE, restriction = Inf) {
    if (!is.list(x)) {
        stop("x must be a list")   
    }
    p <- length(x)
    nms <- names(x)
    if (is.null(nms)) {
        nms <- paste("V", 1:p, sep = "")   
    }
    rslt <- NULL
    for (i in 1:p) {
        if (survival::is.Surv(x[[i]])) {
            rslt <- rbind(rslt, describe_stratified_surv(x[[i]], strata, 
                                                         subset, probs, thresholds, 
                                                         geomInclude, replaceZeroes, 
                                                         restriction))
        } else if (is.Date(x[[i]])) {
            rslt <- rbind(rslt, describe_stratified_date(x[[i]], strata, subset, 
                                                         probs, thresholds, 
                                                         geomInclude, replaceZeroes))
        } else {
            rslt <- rbind(rslt, describe_stratified_vector(x[[i]], strata, subset, 
                                                           probs, thresholds, 
                                                           geomInclude, replaceZeroes))
        }
    }
    dimnames(rslt)[[1]] <- paste(format(rep(nms, each = (dim(rslt)[1])/p)), dimnames(rslt)[[1]])
    rslt
}

#' Print method for class uDescriptives
#' @noRd
#' @export
print.uDescriptives <- function (x, ..., sigfigs=max(3,getOption("digits")-3),width=9,nonsci.limit=5, print.it= TRUE) {
    
    #
    # prints CI or exp(CI) to specified sigfigs
    # rounds exp(Est) to same number of decimal figures as exp(CI)
    # prints Std Err or Robust SE to specified sigfigs
    # rounds Estimate to same number of decimal figures as StdErr or Robust SE
    # prints F stat to specified sigfigs
    # prints P value to number of decimal figures as nonsci.limit unless < 10^-nonsci.limit when "< .00001" used
    # centers all but df and P value, which is right justified
    
    cmptRoundDigits <- function (x, sf) {
        y <- max(abs(x),na.rm=TRUE)
        if (y==0) {
            sf
        } else {
            y <- trunc(log(y) / log(10)) - (y < 1)
            max(0,sf - y - (y < sf))
        }
    }
    
    frmtCol <- function (x, sf, nonsci.limit, colwidth=9, append="") {
        rslt <- NULL
        for (i in 1:length(x)) {	
            if (is.na(x[i])) {
                tmp <- "NA"
            } else {
                rd <- cmptRoundDigits (x[i], sf)
                if (rd <= nonsci.limit & abs(x[i]) < 10^nonsci.limit) {
                    tmp <- format(round(x[i],rd),nsmall=rd,width=1)
                } else {
                    tmp <- format(round(x[i],rd), digits=sf, scientific=TRUE, width=1)
                }
            }
            rslt <- c(rslt,ifelse(x[i]<0,tmp,paste(" ",tmp,sep="")))
        }
        rslt <- paste(rslt,append,sep="")
        format(rslt, justify="centre", width=colwidth)
    }
    
    ncol <- dim(x)[2]
    meancol <- (1:ncol)[dimnames(x)[[2]]=="Mean"]
    mincol <- (1:ncol)[dimnames(x)[[2]]==" Min"]
    maxcol <- (1:ncol)[dimnames(x)[[2]]==" Max"]
    censMin <- x[,"firstEvent"] > x[,mincol]
    censMin[is.na(censMin)] <- FALSE
    censMax <- x[,"lastEvent"] < x[,maxcol]
    censMax[is.na(censMax)] <- FALSE
    if (!any(censMax)) {
        restriction <- NULL
    } else {
        restriction <- frmtCol(x[,"restriction"],sigfigs,nonsci.limit,1,")")
        restriction <- paste("(R",restriction,sep="")
        restriction[!censMax] <- ""
        restriction <- format(restriction, justify="left")
    }
    frmtCoefficients <- format(x[,1:(ncol-4),drop=FALSE])
    for (j in 1:2) frmtCoefficients[,j] <- format (x[,j],justify="right",width=5)
    frmtCoefficients[,mincol] <- frmtCol (x[,mincol],sigfigs,nonsci.limit,width,ifelse(censMin,"+",""))
    for (j in 3:5) frmtCoefficients[,j] <- frmtCol (x[,j],sigfigs,nonsci.limit,width,ifelse(censMax,"+",""))
    if(any(dimnames(x)[[2]] == "Geom Mn")){
        indx <- 7:(ncol-4)
    } else {
        indx <- 6:(ncol-4)
    }
    indx <- indx[indx != maxcol]
    for (j in indx) frmtCoefficients[,j] <- frmtCol (x[,j],sigfigs,nonsci.limit,width)
    frmtCoefficients[,maxcol] <- frmtCol (x[,maxcol],sigfigs,nonsci.limit,width,ifelse(censMax,"+",""))
    ## set a temp variable to check if any are NA
    dateBool <- any(is.na(x[,"isDate"]))
    if(dateBool){
        
    } else {
        if (any(x[,"isDate"]==1)) {
            xformCol <- c(3,5:(dim(x)[2]-4))
            for (j in 1:length(xformCol)) {
                if (substring(dimnames(x)[[2]][xformCol[j]],1,2)=="Pr") xformCol[j] <- NA
            }
            xformCol <- xformCol[!is.na(xformCol)]
            orgn <- "1970-01-01"
            frmtCoefficients[x[,"isDate"]==1,xformCol] <- format(as.Date(x[x[,"isDate"]==1,xformCol],orgn))
        }
    }
    if (!is.null(restriction)) frmtCoefficients <- cbind(frmtCoefficients[,1:2,drop=FALSE],
                                                         "Restrict"=restriction,frmtCoefficients[,-(1:2),drop=FALSE])
    if(print.it) print(frmtCoefficients,quote=FALSE)
    invisible(frmtCoefficients)
}


