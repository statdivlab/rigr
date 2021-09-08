#' Descriptive Statistics
#' 
#' Produces table of relevant descriptive statistics for an arbitrary number of
#' variables of class \code{integer}, \code{numeric}, \code{Surv}, \code{Date},
#' or \code{factor}. Descriptive statistics can be obtained within strata, and
#' the user can specify that only a subset of the data be used. Descriptive
#' statistics include the count of observations, the count of cases with
#' missing values, the mean, standard deviation, geometric mean, minimum, and
#' maximum. The user can specify arbitrary quantiles to be estimated, as well
#' as specifying the estimation of proportions of observations within specified
#' ranges.
#' 
#' This function
#' depends on the \code{survival} R package. You should execute
#' \code{library(survival)} if that library has not been previously installed.
#' Quantiles are computed for uncensored data using the default method in
#' \code{quantile()}. For variables of class \code{factor}, descriptive
#' statistics will be computed using the integer coding for factors. For
#' variables of class \code{Surv}, estimated proportions and quantiles will be
#' computed from Kaplan-Meier estimates, as will be restricted means,
#' restricted standard deviations, and restricted geometric means. For
#' variables of class \code{Date}, estimated proportions will be labeled using
#' the Julian date since January 1, 1970.
#' 
#' @aliases descrip print.uDescriptives
#' @param \dots an arbitrary number of variables for which descriptive statistics 
#'   are desired. The arguments can be vectors, matrices, or lists. Individual 
#'   columns of a matrix or elements of a list may be of class \code{numeric}, 
#'   \code{factor}, \code{Surv}, or \code{Date}. Factor variables are converted 
#'   to integers. Character vectors will be coerced to numeric. Variables may be 
#'   of different lengths, unless \code{strata} or \code{subset} are non-\code{NULL}.
#'   A single \code{data.frame} or \code{tibble} may also be entered, in which case
#'   each variable in the object will be described.
#' @param strata a vector, matrix, or list of stratification variables. Descriptive 
#'   statistics will be computed within strata defined by each unique combination 
#'   of the stratification variables, as well as in the combined sample. 
#'   If \code{strata} is supplied, all variables must be of that same length.
#' @param subset a vector indicating a subset to be used for all descriptive statistics. 
#'   If \code{subset} is supplied, all variables must be of that same length.
#' @param probs a vector of probabilities between 0 and 1 indicating quantile 
#'   estimates to be included in the descriptive statistics. Default is to compute 
#'   25th, 50th (median) and 75th percentiles.
#' @param geomInclude if not \code{FALSE} (the default), includes the geometric mean 
#'   in the descriptive statistics.
#' @param replaceZeroes if not \code{FALSE} (the default), this indicates a value 
#'   to be used in place of zeroes when computing a geometric mean. If \code{TRUE}, 
#'   a value equal to one-half the lowest nonzero value is used. If a numeric value 
#'   is supplied, that value is used for all variables.
#' @param restriction a value used for computing restricted means, standard deviations, 
#'   and geometric means with censored time-to-event data. The default value of 
#'   \code{Inf} will cause restrictions at the highest observation. Note that the 
#'   same value is used for all variables of class \code{Surv}.
#' @param above a vector of values used to dichotomize variables. The descriptive 
#'   statistics will include an estimate for each variable of the proportion of 
#'   measurements with values greater than each element of \code{above}.
#' @param below a vector of values used to dichotomize variables. The descriptive 
#'   statistics will include an estimate for each variable of the proportion of 
#'   measurements with values less than each element of \code{below}.
#' @param labove a vector of values used to dichotomize variables. The descriptive 
#'   statistics will include an estimate for each variable of the proportion of 
#'   measurements with values greater than or equal to each element of \code{labove}.
#' @param rbelow a vector of values used to dichotomize variables. The descriptive 
#'   statistics will include an estimate for each variable of the proportion of 
#'   measurements with values less than or equal to each element of \code{rbelow}.
#' @param lbetween a vector of values with \code{-Inf} and \code{Inf} appended 
#'   is used as cutpoints to categorize variables. The descriptive statistics will 
#'   include an estimate for each variable of the proportion of measurements with 
#'   values between successive elements of \code{lbetween}, with the left-hand 
#'   endpoint included in each interval.
#' @param rbetween a vector of values with \code{-Inf} and \code{Inf} appended 
#'   is used as cutpoints to categorize variables. The descriptive statistics will 
#'   include an estimate for each variable of the proportion of measurements with 
#'   values between successive elements of \code{rbetween}, with the right-hand 
#'   endpoint included in each interval.
#' @param interval a two-column matrix of values in which each row is used to 
#'   define intervals of interest to categorize variables. The descriptive 
#'   statistics will include an estimate for each variable of the proportion of 
#'   measurements with values between two elements in a row, with neither endpoint 
#'   included in each interval.
#' @param linterval a two-column matrix of values in which each row is used to 
#'   define intervals of interest to categorize variables. The descriptive 
#'   statistics will include an estimate for each variable of the proportion of 
#'   measurements with values between two elements in a row, with the left-hand 
#'   endpoint included in each interval.
#' @param rinterval a two-column matrix of values in which each row is used to 
#'   define intervals of interest to categorize variables. The descriptive 
#'   statistics will include an estimate for each variable of the proportion of 
#'   measurements with values between two elements in a row, with the right-hand 
#'   endpoint included in each interval.
#' @param lrinterval a two-column matrix of values in which each row is used to 
#'   define intervals of interest to categorize variables. The descriptive 
#'   statistics will include an estimate for each variable of the proportion of 
#'   measurements with values between two elements in a row, with both endpoints 
#'   included in each interval.
#' @return An object of class \code{uDescriptives} is returned. Descriptive 
#'   statistics for each variable in the entire subsetted sample, as well as 
#'   within each stratum if any is defined, are contained in a matrix with rows 
#'   corresponding to variables and strata and columns corresponding to the 
#'   descriptive statistics. Descriptive statistics include
#'   \itemize{
#'     \item{N:} { the number of observations.} 
#'     \item{Msng:} { the number of observations with missing values.}
#'     \item{Mean:} { the mean of the nonmissing observations (this is potentially 
#'                   a restricted mean for right-censored time-to-event data).} 
#'     \item{Std Dev:} { the standard deviation of the nonmissing observations 
#'                     (this is potentially a restricted standard deviation for 
#'                     right-censored time to event data).} 
#'     \item{Geom Mn:} { the geometric mean of the nonmissing observations 
#'                    (this is potentially a restricted geometric mean for 
#'                    right-censored time to event data). Nonpositive values in 
#'                    the variable will generate \code{NA}, unless \code{replaceZeroes} 
#'                    was specified.}
#'     \item{Min:} { the minimum value of the nonmissing observations (this is
#'                  potentially restricted for right-censored time-to-event data).}
#'     \item{Quantiles:} { columns corresponding to the quantiles specified by \code{probs} 
#'                      (these are potentially restricted for right-censored 
#'                      time-to-event data).}
#'     \item{Max:} { the maximum value of the nonmissing observations (this is
#'                  potentially restricted for right-censored time-to-event data).}
#'     \item{Proportions:} { columns corresponding to the proportions as specified by
#'                         \code{above}, \code{below}, \code{labove}, \code{rbelow}, 
#'                         \code{lbetween}, \code{rbetween}, \code{interval}, 
#'                         \code{linterval}, \code{rinterval}, and \code{lrinterval}.} 
#'     \item{restriction:} { the threshold for restricted means, standard deviations, 
#'                          and geometric means.} 
#'     \item{firstEvent:} { the time of the first event for censored time-to-event variables.} 
#'     \item{lastEvent:} { the time of the last event for censored time-to-event variables.} 
#'     \item{isDate:} { an indicator that the variable is a \code{Date} object.}
#'   } 
#' 
#' @examples
#' 
#' # Read in the data
#' data(mri) 
#' 
#' # Create the table 
#' descrip(mri)
#' 
#' @export descrip
descrip <- function (..., strata = NULL, subset = NULL, 
          probs = c(0.25, 0.5, 0.75), geomInclude = FALSE, replaceZeroes = FALSE, 
          restriction = Inf, above = NULL, below = NULL, labove = NULL, rbelow = NULL, 
          lbetween = NULL, rbetween = NULL, interval = NULL, linterval = NULL, 
          rinterval = NULL, lrinterval = NULL) {
  
  # Get list of descriptive variables
  L <- list(...)
  
  # Get names of descriptive variables
  names(L) <- unlist(match.call(expand.dots = FALSE)$...)
  
  # if L is length 1 and a tibble, data.frame, or matrix, unlist
  if ((length(L) == 1) & any(grepl("data.frame", class(L[[1]])))) {
    L <- L[[1]]
  }
  
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
        for (i in 2:length(strata)) {
          tmp <- paste(tmp, snms[i], format(strata[[i]]))
        }
      }
    }
    else {
      strata <- as.matrix(strata)
      snms <- dimnames(strata)[[2]]
      if (is.null(snms)) {
        snms <- rep("", dim(strata)[2])
      }
      tmp <- paste(snms[1], format(strata[, 1]))
      if (dim(strata)[2] > 1) {
        for (i in 2:(dim(strata)[2])) {
          tmp <- paste(tmp, snms[i], format(strata[, i]))
        }
      }
    }
    strata <- tmp
  }
  
  # if thresholds are not vectors, throw an error
  if (!is.atomic(above) & !is.null(above)) {
    stop("above must be a vector")
  }
  if (!is.atomic(below) & !is.null(below)) {
    stop("below must be a vector")
  }
  if (!is.atomic(labove) & !is.null(labove)) {
    stop("labove must be a vector")
  }
  if (!is.atomic(rbelow) & !is.null(rbelow)) {
    stop("rbelow must be a vector")
  }
  if (!is.atomic(lbetween) & !is.null(lbetween)) {
    stop("lbetween must be a vector")
  }
  if (!is.atomic(rbetween) & !is.null(rbetween)) {
    stop("rbetween must be a vector")
  }
  
  # if thresholds are not numeric, give warning
  if (!is.numeric(above) & !is.null(above)) {
    stop("above must be numeric")
  }
  if (!is.numeric(below) & !is.null(below)) {
    stop("below must be numeric")
  }
  if (!is.numeric(labove) & !is.null(labove)) {
    stop("labove must be numeric")
  }
  if (!is.numeric(rbelow) & !is.null(rbelow)) {
    stop("rbelow must be numeric")
  }
  if (!is.numeric(lbetween) & !is.null(lbetween)) {
    stop("lbetween must be numeric")
  }
  if (!is.numeric(rbetween) & !is.null(rbetween)) {
    stop("rbetween must be numeric")
  }
  
  # Calculate the thresholds
  thresholds <- NULL
  if (length(above) > 0) {
    thresholds <- rbind(thresholds, cbind(0, above, 0, Inf))
  }
  if (length(labove) > 0) {
    thresholds <- rbind(thresholds, cbind(1, labove, 0, Inf))
  }
  if (length(below) > 0) {
    thresholds <- rbind(thresholds, cbind(0, -Inf, 0, below))
  }
  if (length(rbelow) > 0) {
    thresholds <- rbind(thresholds, cbind(0, -Inf, 1, rbelow))
  }
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
  
  # Add interval, linterval, rinterval, rlinterval to thresholds
  
  if (!is.null(interval)) {
    if (length(interval) == 2) {
      interval <- matrix(interval, ncol = 2)
    }
      
    # add option to input interval as a vector
    if (is.vector(interval)) {
      interval <- sort(unique(interval))
      interval <- matrix(c(interval[-length(interval)], interval[-1]), 
                         ncol = 2, byrow = FALSE)
      
      interval_warning <- paste0("Assuming intervals between points specified in vector. ", 
                                 "To specify specific intervals, enter interval argument as ", 
                                 "a 2 column matrix instead of a vector")
      
      warning(interval_warning)
    }
    
    # if matrix has more than 2 columns, throw an error
    if (dim(interval)[2] != 2) {
      stop("interval must be specified in a 2 column matrix")
    }
    
    # add interval to thresholds
    thresholds <- rbind(thresholds, cbind(0, interval[, 1], 
                                          0, interval[, 2]))
  }
  
  if (!is.null(linterval)) {
    if (length(linterval) == 2) {
      linterval <- matrix(linterval, ncol = 2)
    }
    
    # add option to input linterval as a vector  
    if (is.vector(linterval)) {
      linterval <- sort(unique(linterval))
      linterval <- matrix(c(linterval[-length(linterval)], linterval[-1]), 
                          ncol = 2, byrow = FALSE)
      
      linterval_warning <- paste0("Assuming lintervals between points specified in vector. ", 
                                 "To specify specific lintervals, enter linterval argument as ", 
                                 "a 2 column matrix instead of a vector")
    
      warning(linterval_warning)
    }
    
    # if matrix has more than 2 columns, throw an error
    if (dim(linterval)[2] != 2) {
      stop("linterval must be specified in a 2 column matrix")
    }
      
    # add to thresholds
    thresholds <- rbind(thresholds, cbind(1, linterval[, 1], 0, linterval[, 2]))
  }
  
  if (!is.null(rinterval)) {
    if (length(rinterval) == 2) {
      rinterval <- matrix(rinterval, ncol = 2)
    }
      
    # add option to input rinterval as a vector  
    if (is.vector(rinterval)) {
      rinterval <- sort(unique(rinterval))
      rinterval <- matrix(c(rinterval[-length(rinterval)], rinterval[-1]), 
                          ncol = 2, byrow = FALSE)
      
      rinterval_warning <- paste0("Assuming rintervals between points specified in vector. ", 
                                  "To specify specific rintervals, enter rinterval argument as ", 
                                  "a 2 column matrix instead of a vector")
      
      warning(rinterval_warning)
    }
    
    # if matrix has more than 2 columns, throw an error
    if (dim(rinterval)[2] != 2) {
      stop("rinterval must be specified in a 2 column matrix")
    }
      
    # add to thresholds
    thresholds <- rbind(thresholds, cbind(0, rinterval[,1], 1, rinterval[, 2]))
  }
  
  if (!is.null(lrinterval)) {
    if (length(lrinterval) == 2) {
      lrinterval <- matrix(lrinterval, ncol = 2)
    }
      
    # add option to input lrinterval as a vector 
    if (is.vector(lrinterval)) {
      lrinterval <- sort(unique(lrinterval))
      lrinterval <- matrix(c(lrinterval[-length(lrinterval)], lrinterval[-1]),
                           ncol = 2, byrow = FALSE)
      
      lrinterval_warning <- paste0("Assuming lrintervals between points specified in vector. ", 
                                  "To specify specific lrintervals, enter lrinterval argument as ", 
                                  "a 2 column matrix instead of a vector")
      
      warning(lrinterval_warning)
    }
    
    # if matrix has more than 2 columns, throw an error
    if (dim(lrinterval)[2] != 2) {
      stop("lrinterval must be specified in a 2 column matrix")
    }
      
    # add to thresholds
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
      rslt <- rbind(rslt, describe_stratified_list(x, strata, subset, probs, 
                                                   thresholds, geomInclude, 
                                                   replaceZeroes, restriction))
    }
    else if (is.matrix(x) & !survival::is.Surv(x)) {
      dimnames(x) <- list(NULL, nms[nV + (1:(dim(x)[2]))])
      nV <- nV + dim(x)[2]
      rslt <- rbind(rslt, describe_stratified_matrix(x, strata, subset, probs, 
                                                     thresholds, geomInclude, 
                                                     replaceZeroes))
    }
    else if (survival::is.Surv(x)) {
      nV <- nV + 1
      rslt <- rbind(rslt, describe_stratified_surv(x, strata, subset, probs, 
                                                   thresholds, geomInclude, 
                                                   replaceZeroes, restriction))
      rownames(rslt)[nV] <- nms[nV]
    }
    else if (is.Date(x)) {
      nV <- nV + 1
      rslt <- rbind(rslt, describe_stratified_date(x, strata, subset, probs, 
                                                   thresholds, geomInclude, replaceZeroes))
      rownames(rslt)[nV] <- nms[nV]
    }
    else if (is.factor(x)) {
      x <- list(x)
      nV <- nV + 1
      names(x) <- nms[nV]
      rslt <- rbind(rslt, describe_stratified_list(x, strata, subset, probs, 
                                                   thresholds, geomInclude, replaceZeroes))
    }
    else {
      x <- matrix(x)
      nV <- nV + 1
      dimnames(x) <- list(NULL, nms[nV])
      rslt <- rbind(rslt, describe_stratified_matrix(x, strata, subset, probs, 
                                                     thresholds, geomInclude, replaceZeroes))
    }
  }
  class(rslt) <- "uDescriptives"
  rslt
}
