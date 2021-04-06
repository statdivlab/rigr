descrip <-
function (..., strata = NULL, subset = NULL, probs = c(0.25, 
                                                       0.5, 0.75), geomInclude = FALSE, replaceZeroes = FALSE, restriction = Inf, 
          above = NULL, below = NULL, labove = NULL, rbelow = NULL, 
          lbetween = NULL, rbetween = NULL, interval = NULL, linterval = NULL, 
          rinterval = NULL, lrinterval = NULL, version = FALSE) 
{
  vrsn <- "20160730"
  if (version) 
    return(vrsn)
  is.Date <- function(x) inherits(x, "Date")
  # Begin vDescrip
  vDescrip <- function(x, probs, thresholds, geometricMean, 
                       geomInclude, replaceZeroes) {
    if (is.factor(x) | is.logical(x)) {
      x <- as.numeric(x)
    }
    if (!geomInclude) {
      geometricMean <- geomInclude
    }# would geometricMean<-geomInclude not work here? False if geometricInclude ==FALSE
    ntholds <- ifelse(is.null(thresholds), 0, dim(thresholds)[1])
    probs <- sort(unique(c(probs, 0, 1)))
    rslt <- length(x)
    if (rslt == 0) {
      rslt <- c(rslt, rep(NaN, 7 + length(probs) + ntholds))
    }
    else {
      u <- is.na(x)
      rslt <- c(rslt, sum(u))
      x <- x[!u]
      if (length(x) == 0 | is.character(x)) {
        if (!geomInclude) {
          rslt <- c(rslt, rep(NA, 6 + length(probs) + ntholds))
        }
        else {
          rslt <- c(rslt, rep(NA, 7 + length(probs) + ntholds))
        }
      }
      else {
        if (geomInclude) {
          rslt <- c(rslt, mean(x), sd(x), ifelse1(geometricMean, exp(mean(log(ifelse(x == 0, replaceZeroes, x)))), NA), quantile(x, probs))
        }
        else {
          rslt <- c(rslt, mean(x), sd(x), quantile(x, 
                                                   probs))
        }
        if (ntholds > 0) {
          for (j in 1:ntholds) {
            u <- ifelse1(thresholds[j, 1] == 0, x > thresholds[j, 
                                                               2], x >= thresholds[j, 2]) & ifelse1(thresholds[j, 
                                                                                                               3] == 0, x < thresholds[j, 4], x <= thresholds[j, 
                                                                                                                                                              4])
            rslt <- c(rslt, mean(u))
          }
        }
        rslt <- c(rslt, Inf, rslt[5 + c(1, length(probs))])
      }
    }
    if (length(x) > 0) {
      rslt <- matrix(c(rslt, 0), 1)
    }
    else {
      rslt <- matrix(rslt, 1)
    }
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
  # end vDescrip
  
  # begin sDescrip
  sDescrip <- function(x, probs, thresholds, geomInclude, geometricMean, 
                       replaceZeroes, restriction) {
    # under sDescrip, begin KM
    KM <- function(x) {
      if (!survival::is.Surv(x)) 
        stop("x must be a Surv object")
      x <- x[!is.na(x)]
      obs <- x[, 1]
      ev <- x[, 2]
      ce <- 1 - ev
      if (length(obs) == 0) 
        stop("No data to estimate survival curve")
      N <- length(obs)
      if (!any(obs == 0)) {
        obs <- c(0, obs)
        ev <- c(0, ev)
        ce <- c(0, ce)
      }
      i <- order(obs, 1 - ev)
      obs <- obs[i]
      ev <- ev[i]
      ce <- ce[i]
      ev <- rev(cumsum(rev(ev)))
      ce <- rev(cumsum(rev(ce)))
      n <- ce + ev
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
    # under sDescrip, end KM
    
    # under sDescrip, begin sKM
    sKM <- function(x, times, rightCtsCDF = T) {
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
    # under sDescrip, end sKM
    
    # under sDescrip, begin pKM
    pKM <- function(x, times, rightCtsCDF = T) {
      1 - sKM(x, times, rightCtsCDF)
    }
    # under sDescrip, end pKM
    
    # under sDescrip, begin qKM
    qKM <- function(x, probs) {
      rslt <- length(probs)
      for (i in 1:length(probs)) {
        p <- 1 - probs[i]
        j <- abs(x$S - p) < 1e-15 & x$events > 0
        if (any(j)) {
          if (abs(p - min(x$S)) < 1e-15) {
            rslt[i] <- (x$t[j] + max(x$t))/2
          }
          else {
            rslt[i] <- (x$t[j] + min(x$t[x$t > x$t[j] & 
                                           x$events > 0]))/2
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
    # under sDescrip, end qKM
    
    #under sDescrip, begin meanKM
    meanKM <- function(x, restriction) {
      if (length(restriction) == 1) 
        restriction <- c(x$t[1] - 1, restriction)
      if (restriction[2] == Inf) 
        restriction[2] <- x$t[length(x$t)]
      tms <- c(restriction[1], x$t[x$t > restriction[1] & 
                                     x$t < restriction[2]], restriction[2])
      s <- sKM(x, restriction)
      s <- c(s[1], x$S[x$t > restriction[1] & x$t < restriction[2]], 
             s[2])
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
    # under sDescrip, end mean KM
    
    # after declaring KM functions, begin sDescrip
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
        tmp1 <- meanKM(z, restriction)
        x2 <- x
        x2[, 1] <- x2[, 1]^2
        z2 <- KM(x2)
        tmp2 <- sqrt(meanKM(z2, restriction^2) - tmp1^2)
        if (geometricMean) {
          x2 <- x
          x2[, 1] <- ifelse(x2[, 1] == 0, log(replaceZeroes), 
                            log(x2[, 1]))
          z2 <- KM(x2)
          tmp3 <- exp(meanKM(z2, log(restriction)))
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
                    qKM(z, probs[-c(1, length(probs))]), max(x[, 
                                                               1]))
        }
        else {
          rslt <- c(rslt, tmp1, tmp2, min(x[, 1]), qKM(z, 
                                                       probs[-c(1, length(probs))]), max(x[, 1]))
        }
        if (ntholds > 0) {
          for (j in 1:ntholds) {
            rslt <- c(rslt, ifelse1(thresholds[j, 1] == 
                                      0, sKM(z, thresholds[j, 2]), sKM(z, thresholds[j, 
                                                                                     2], F)) - ifelse1(thresholds[j, 4] == Inf, 
                                                                                                       0, ifelse1(thresholds[j, 3] == 0, sKM(z, 
                                                                                                                                             thresholds[j, 4], F), sKM(z, thresholds[j, 
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
  # end sDescrip
  
  # begin vStrdescr
  vStrdescr <- function(x, stratav, subsetv, probs, thresholds, 
                        geomInclude, replaceZeroes) {
    if (is.null(subsetv)) 
      subsetv <- rep(T, length(x))
    if (length(x) != length(subsetv)) 
      stop("length of variables must match length of subset")
    if (is.null(stratav)) 
      stratav <- rep(1, length(x))
    if (length(x) != length(stratav)) 
      stop("length of variables must match length of strata")
    x <- x[subsetv]
    if (is.factor(x) | all(x[!is.na(x)] %in% c(0, 1))) {
      geometricMean <- FALSE
    }
    else {
      geometricMean <- !any(x[!is.na(x)] < 0)
    }
    if (is.logical(replaceZeroes)) {
      if (!replaceZeroes | is.factor(x) | all(x[!is.na(x)] %in% 
                                              c(0, 1))) {
        replaceZeroes <- NA
      }
      else {
        replaceZeroes <- min(x[!is.na(x) & x > 0])/2
      }
    }
    stratav <- stratav[subsetv]
    s <- sort(unique(stratav))
    rslt <- vDescrip(x, probs, thresholds, geometricMean, 
                     geomInclude, replaceZeroes)
    if (length(s) > 1) {
      for (i in s) rslt <- rbind(rslt, vDescrip(x[stratav == 
                                                    i & !is.na(stratav)], probs, thresholds, geometricMean, 
                                                geomInclude, replaceZeroes))
      if (any(is.na(stratav))) {
        rslt <- rbind(rslt, vDescrip(x[is.na(stratav)], 
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
  # end vStrdescr
  
  # begin dStrdescr
  dStrdescr <- function(x, stratad, subsetd, probs, threshholds, 
                        geomInclude, replaceZeroes) {
    if (!is.Date(x)) 
      stop("x must be a Date object")
    xi <- as.integer(x)
    rslt <- vStrdescr(xi, stratad, subsetd, probs, thresholds, 
                      geomInclude, replaceZeroes)
    rslt[, "isDate"] <- 1
    rslt
  }
  # end dStrdescr
  
  # begin sStrdescr
  sStrdescr <- function(x, stratas, subsets, probs, thresholds, 
                        geomInclude, replaceZeroes, restriction) {
    if (!survival::is.Surv(x)) 
      stop("x must be a Surv object")
    n <- dim(x)[1]
    if (is.null(subsets)) 
      subsets <- rep(T, n)
    if (n != length(subsets)) 
      stop("length of variables must match length of subset")
    if (is.null(stratas)) 
      stratas <- rep(1, n)
    if (n != length(stratas)) 
      stop("length of variables must match length of strata")
    x <- x[subsets]
    if (geomInclude) {
      geometricMean <- !any(x[!is.na(x), 1] < 0)
    }
    else {
      geometricMean <- FALSE
    }
    if (is.logical(replaceZeroes)) {
      if (replaceZeroes) 
        replaceZeroes <- min(x[!is.na(x) & x[, 1] > 0, 
                               1])/2
      else replaceZeroes <- NA
    }
    stratas <- stratas[subsets]
    s <- sort(unique(stratas))
    rslt <- sDescrip(x, probs, thresholds, geomInclude, geometricMean, 
                     replaceZeroes, restriction)
    if (length(s) > 1) {
      for (i in s) rslt <- rbind(rslt, sDescrip(x[stratas == 
                                                    i & !is.na(stratas)], probs, thresholds, geomInclude, 
                                                geometricMean, replaceZeroes, restriction))
      if (any(is.na(stratas))) {
        rslt <- rbind(rslt, sDescrip(x[is.na(stratas)], 
                                     probs, thresholds, geomInclude, geometricMean, 
                                     replaceZeroes, restriction))
        dimnames(rslt)[[1]] <- format(c("All", paste("  Str", 
                                                     format(c(format(s), "NA")))))
      }
      else dimnames(rslt)[[1]] <- format(c("All", paste("  Str", 
                                                        format(s))))
    }
    rslt
  }
  # end sStrdescr
  
  #begin mStrdescr
  mStrdescr <- function(x, stratam, subsetm, probs, thresholds, 
                        geomInclude, replaceZeroes) {
    if (!is.matrix(x)) 
      stop("x must be a matrix")
    p <- dim(x)[2]
    nms <- dimnames(x)[[2]]
    if (is.null(nms)) 
      nms <- paste("V", 1:p, sep = "")
    rslt <- NULL
    for (i in 1:p) {
      rslt <- rbind(rslt, vStrdescr(x[, i], stratam, subsetm, 
                                    probs, thresholds, geomInclude, replaceZeroes))
    }
    dimnames(rslt)[[1]] <- paste(format(rep(nms, each = (dim(rslt)[1])/p)), 
                                 dimnames(rslt)[[1]])
    rslt
  }
  # end mStrdescr
  
  # begin lStrdescr
  lStrdescr <- function(x, stratal, subsetl, probs, thresholds, 
                        geomInclude, replaceZeroes, restriction) {
    if (!is.list(x)) 
      stop("x must be a list")
    p <- length(x)
    nms <- names(x)
    if (is.null(nms)) 
      nms <- paste("V", 1:p, sep = "")
    rslt <- NULL
    for (i in 1:p) {
      if (survival::is.Surv(x[[i]])) {
        rslt <- rbind(rslt, sStrdescr(x[[i]], stratal, 
                                      subsetl, probs, thresholds, geomInclude, replaceZeroes, 
                                      restriction))
      }
      else if (is.Date(x[[i]])) {
        rslt <- rbind(rslt, dStrdescr(x[[i]], stratal, 
                                      subsetl, probs, thresholds, geomInclude, replaceZeroes))
      }
      else {
        rslt <- rbind(rslt, vStrdescr(x[[i]], stratal, 
                                      subsetl, probs, thresholds, geomInclude, replaceZeroes))
      }
    }
    dimnames(rslt)[[1]] <- paste(format(rep(nms, each = (dim(rslt)[1])/p)), 
                                 dimnames(rslt)[[1]])
    rslt
  }
  # end lStrdescr
  
  # begin Descrip()
  
  # Get list of descriptive variables
  L <- list(...)
  
  # Get names of descriptive variables
  names(L) <- unlist(match.call(expand.dots = F)$...)
  
  # p gets how many descriptive variables
  p <- length(L)
  nms <- NULL
  
  # for each descriptive variable (i in 1:p), name it 
  for (i in 1:p) {
    # x gets data for that variable
    x <- L[[i]]
    
    # if descriptive variable is a list
    if (is.list(x)) {
      # and there are no names
      if (is.null(names(x))) {
        # give it names in the form name.Vk where k is the index
        nms <- c(nms, paste(names(L)[i], ".V", 1:length(x), 
                            sep = ""))
      }
      # if there are names, just use the ones it's already given
      else nms <- c(nms, names(x))
    }
    # if x is a matrix and not a survival
    else if (is.matrix(x) & !survival::is.Surv(x)) {
      if (is.null(dimnames(x)[[2]])) {
        nms <- c(nms, paste(names(L)[i], ".V", 1:(dim(x)[2]), 
                            sep = ""))
      }
      else nms <- c(nms, dimnames(x)[[2]])
    }
    else nms <- c(nms, names(L)[i])
  }
  
  # print the names we just created
  nms <- paste(format(nms, justify = "right"), ": ", sep = "")
  
  # Look for stratification
  if (!is.null(strata)) {
    # Ensure stratifiation is correctly input
    if (is.list(strata)) {
      for (i in 1:length(strata)) {
        if (!is.vector(strata[[i]])) 
          stop("strata can only be a vector, matrix, or list of vectors")
      }
      n <- length(strata[[1]])
      if (length(strata) > 1) {
        for (i in 2:length(strata)) {
          if (length(strata[[i]]) != n) 
            stop("all elements in strata must be same length")
        }
      }
      # get names for each strata
      snms <- names(strata)
      if (is.null(snms)) 
        snms <- rep("", length(strata))
      tmp <- paste(snms[1], format(strata[[1]]))
      if (length(strata) > 1) {
        for (i in 2:length(strata)) tmp <- paste(tmp, 
                                                 snms[i], format(strata[[i]]))
      }
    }
    else {
      strata <- as.matrix(strata)
      snms <- dimnames(strata)[[2]]
      if (is.null(snms)) 
        snms <- rep("", dim(strata)[2])
      tmp <- paste(snms[1], format(strata[, 1]))
      if (dim(strata)[2] > 1) {
        for (i in 2:(dim(strata)[2])) tmp <- paste(tmp, 
                                                   snms[i], format(strata[, i]))
      }
    }
    strata <- tmp
  }
  
  # Calculate the thresholds
  thresholds <- NULL
  if (length(above) > 0) 
    thresholds <- rbind(thresholds, cbind(0, above, 0, Inf))
  if (length(labove) > 0) 
    thresholds <- rbind(thresholds, cbind(1, labove, 0, Inf))
  if (length(below) > 0) 
    thresholds <- rbind(thresholds, cbind(0, -Inf, 0, below))
  if (length(rbelow) > 0) 
    thresholds <- rbind(thresholds, cbind(0, -Inf, 1, rbelow))
  if (length(lbetween) > 0) {
    lbetween <- sort(unique(c(-Inf, lbetween, Inf)))
    thresholds <- rbind(thresholds, cbind(1, lbetween[-length(lbetween)], 
                                          0, lbetween[-1]))
  }
  if (length(rbetween) > 0) {
    rbetween <- sort(unique(c(-Inf, rbetween, Inf)))
    thresholds <- rbind(thresholds, cbind(0, rbetween[-length(rbetween)], 
                                          1, rbetween[-1]))
  }
  if (!is.null(interval)) {
    if (length(interval) == 2) 
      interval <- matrix(interval, ncol = 2)
    # add option to input interval as a vector
    if (is.vector(interval)) 
    {
      interval<-sort(unique(interval))
      interval <- matrix(c(interval[-length(interval)],interval[-1]),ncol=2,byrow=F)
      warning("Assuming intervals between points specified in vector. To specify specific intervals, enter interval argument as a 2 column matrix instead of a vector")
    }
    if (dim(interval)[2] != 2) 
      stop("intervals must be specified in a 2 column matrix")
    thresholds <- rbind(thresholds, cbind(0, interval[, 1], 
                                          0, interval[, 2]))
  }
  if (!is.null(linterval)) {
    if (length(linterval) == 2) 
      linterval <- matrix(linterval, ncol = 2)
    if (is.vector(linterval)) 
    {
      interval<-sort(unique(linterval))
      linterval <- matrix(c(interval[-length(interval)],interval[-1]),ncol=2,byrow=F)
      warning("Assuming intervals between points specified in vector. To specify specific intervals, enter interval argument as a 2 column matrix instead of a vector")
    }
    if (dim(linterval)[2] != 2) 
      stop("intervals must be specified in a 2 column matrix")
    thresholds <- rbind(thresholds, cbind(1, linterval[, 
                                                       1], 0, linterval[, 2]))
  }
  if (!is.null(rinterval)) {
    if (length(rinterval) == 2) 
      rinterval <- matrix(rinterval, ncol = 2)
    if (is.vector(rinterval)) {
      interval<-sort(unique(rinterval))
      rinterval <- matrix(c(interval[-length(interval)],interval[-1]),ncol=2,byrow=F)
      warning("Assuming intervals between points specified in vector. To specify specific intervals, enter interval argument as a 2 column matrix instead of a vector")
    }
    if (dim(rinterval)[2] != 2) 
      stop("intervals must be specified in a 2 column matrix")
    thresholds <- rbind(thresholds, cbind(0, rinterval[,1], 1, rinterval[, 2]))
  }
  if (!is.null(lrinterval)) {
    if (length(lrinterval) == 2) 
      lrinterval <- matrix(lrinterval, ncol = 2)
    if (is.vector(lrinterval)) 
    {
      interval<-sort(unique(lrinterval))
      lrinterval <- matrix(c(interval[-length(interval)],interval[-1]),ncol=2,byrow=F)
      warning("Assuming intervals between points specified in vector. To specify specific intervals, enter interval argument as a 2 column matrix instead of a vector")
    }
    if (dim(lrinterval)[2] != 2) 
      stop("intervals must be specified in a 2 column matrix")
    thresholds <- rbind(thresholds, cbind(1, lrinterval[,1], 1, lrinterval[, 2]))
  }
  
  # Now that we have the labels, get the data 
  # Depending on what type the data is, use appropriate descrip function
  rslt <- NULL
  nV <- 0
  for (i in 1:p) {
    x <- L[[i]]
    if (is.list(x)) {
      names(x) <- nms[nV + (1:length(x))]
      nV <- nV + length(x)
      rslt <- rbind(rslt, lStrdescr(x, strata, subset, 
                                    probs, thresholds, geomInclude, replaceZeroes, 
                                    restriction))
    }
    else if (is.matrix(x) & !survival::is.Surv(x)) {
      dimnames(x) <- list(NULL, nms[nV + (1:(dim(x)[2]))])
      nV <- nV + dim(x)[2]
      rslt <- rbind(rslt, mStrdescr(x, strata, subset, 
                                    probs, thresholds, geomInclude, replaceZeroes))
    }
    else if (survival::is.Surv(x)) {
      nV <- nV + 1
      rslt <- rbind(rslt, sStrdescr(x, strata, subset, 
                                    probs, thresholds, geomInclude, replaceZeroes, 
                                    restriction))
    }
    else if (is.Date(x)) {
      nV <- nV + 1
      rslt <- rbind(rslt, dStrdescr(x, strata, subset, 
                                    probs, thresholds, geomInclude, replaceZeroes))
    }
    else if (is.factor(x)) {
      x <- list(x)
      nV <- nV + 1
      names(x) <- nms[nV]
      rslt <- rbind(rslt, lStrdescr(x, strata, subset, 
                                    probs, thresholds, geomInclude, replaceZeroes))
    }
    else {
      x <- matrix(x)
      nV <- nV + 1
      dimnames(x) <- list(NULL, nms[nV])
      rslt <- rbind(rslt, mStrdescr(x, strata, subset, 
                                    probs, thresholds, geomInclude, replaceZeroes))
    }
  }
  class(rslt) <- "uDescriptives"
  rslt
}
