#' T-test with Improved Layout
#' 
#' Performs a one- or two-sample t-test using data. In the two-sample case, the user can specify
#' whether or not observations are matched, and whether or not equal variances
#' should be presumed.
#' 
#' Missing values must be given by \code{NA} to be recognized as missing values. 
#' 
#' @aliases ttest ttest.do print.ttest ttest.default
#' 
#' @param var1 a (non-empty) numeric vector of
#' data values.
#' @param var2 an optional (non-empty) numeric
#' vector of data.
#' @param by a variable of equal length to
#' that of \code{var1} with two outcomes. This will be used to define strata
#' for a t-test on \code{var1}.
#' @param geom a logical indicating whether the geometric mean should be
#' calculated and displayed.
#' @param null.hypoth a number specifying the
#' null hypothesis for the mean (or difference in means if performing a
#' two-sample test). Defaults to zero.
#' @param alternative a string: one of
#' \code{"less"}, \code{"two.sided"}, or \code{"greater"} specifying the form
#' of the test. Defaults to a two-sided test.
#' @param var.eq a logical value, either
#' \code{TRUE} or \code{FALSE} (default), specifying whether or not equal
#' variances should be presumed in a two-sample t-test. Also controls robust
#' standard errors.
#' @param conf.level confidence level of the
#' test. Defaults to 0.95.
#' @param matched a logical value, either
#' \code{TRUE} or \code{FALSE}, indicating whether or not the variables of a
#' two-sample t-test are matched. Variables must be of equal length.
#' @param more.digits a numeric value
#' specifying whether or not to display more or fewer digits in the output.
#' Non-integers are automatically rounded down. 
#' 
#' @return a list of class \code{ttest}. The print method lays out the information in an easy-to-read
#' format. 
#' \item{tab}{A formatted table of descriptive and inferential statistics (total number of observations,
#' number of missing observations, mean, standard error of the mean estimate, standard deviation), 
#' along with a confidence interval for the mean.}
#' \item{df}{Degrees of freedom for the t-test.}
#' \item{p}{P-value for the t-test.}
#' \item{tstat}{Test statistic for the t-test.}
#' \item{var1}{The user-supplied first data vector. }
#' \item{var2}{The user-supplied second data vector. }
#' \item{by}{The user-supplied stratification variable.}
#' \item{par}{A vector of information about the type of test (null hypothesis, alternative hypothesis, etc.)}
#' \item{geo}{A formatted table of descriptive and inferential statistics for the geometric mean.}
#' \item{call}{The call made to the \code{ttest} function.}
#' 
#' @seealso \code{\link[stats]{t.test}}
#' 
#' @examples
#' 
#' # Read in data set
#' data(psa)
#' attach(psa)
#' 
#' # Perform t-test
#' ttest(pretxpsa, null.hypoth = 100, alternative = "greater", more.digits = 1)
#' 
#' # Define new binary variable as indicator
#' # of whether or not bss was worst possible
#' bssworst <- bss
#' bssworst[bss == 1] <- 0
#' bssworst[bss == 2] <- 0
#' bssworst[bss == 3] <- 1
#' 
#' # Perform t-test allowing for unequal
#' # variances between strata -#
#' ttest(pretxpsa, by = bssworst)
#' 
#' # Perform matched t-test
#' ttest(pretxpsa, nadirpsa, matched = TRUE, conf.level = 99/100, more.digits = 1)
#' 
#' 
#' @export ttest
ttest<-function (var1, var2 = NA, by = NA, geom = FALSE, 
          null.hypoth = 0, alternative = "two.sided", 
          var.eq = FALSE, conf.level = 0.95, matched = FALSE, more.digits = 0) 
{
  ttest.do <- function(var1, var2 = NA, by = NA, geom = FALSE, 
                       null.hypoth = 0, alternative = "two.sided", 
                       var.eq = FALSE, conf.level = 0.95, matched = FALSE, more.digits = 0, 
                       myargs, ...) {
    # geometric mean check (only possible for positive nonzero data)
    if (geom == TRUE & (sum(var1 <= 0, na.rm = TRUE) > 0) | 
        geom == TRUE & sum(var2 <= 0, na.rm = TRUE) > 0) {
      stop("Geometric mean requires that all numeric data are positive")
    }
    # for geometric mean, check to make sure null hypothesis is valid
    if (geom == TRUE & null.hypoth < 0) {
      stop("Geometric mean cannot be less than zero")
    }
    # can only do var1 vs var2 or var1 by
    if (length(var2) > 1 & length(by) > 1) {
      stop("Please specify only one of the variables 'by' or 'var2'")
    }
    # check that alternative is one of "less", "two.sided", or "greater"
    if (!(alternative %in% c("less", "two.sided", "greater"))) {
      stop("'alternative' must be either 'less', 'two.sided', or 'greater'")
    }
    # make sure var.eq is either true or false
    if (!is.logical(var.eq)) {
      stop("Please specify a logical value for variable 'var.eq'")
    }
    if (!(is.scalar(null.hypoth))){
      stop("Null must be a scalar.")
    }
    if (!((length(conf.level) == 1L) && is.finite(conf.level) && 
          (conf.level > 0) && (conf.level < 1))){
      stop("'conf.level' must be a single number between 0 and 1")
    } 
    # get length of stratification variable (byt)
    byt <- sort(unique(by),decreasing=FALSE)
    byt <- byt[!is.na(byt)]
    # make sure var1 and var2 are the same length if a paired t test is requested
    if (matched && (length(var2) > 1)) {
      if (length(var1) != length(var2)) {
        stop("Cannot perform matched t-test on variable of unequal length")
      }
    }
    # make sure data is numeric
    if (!is.numeric(var1) | (!is.numeric(var2) & length(var2) != 
                             1)) {
      stop("Cannot perform t-test on non-numeric data")
    }
    # check to make sure additional digit request is numeric
    if (!is.numeric(more.digits)) {
      stop("Argument 'more.digits' must be numeric")
    }
    # if by is a factor, turn it into a numeric
    if (is.factor(by)) {
      by <- as.numeric(by)
    }
    # something with more digits
    more.digits <- max(floor(more.digits), 0)
    # ensure by variable has length 2 (no more or less)
    if (length(unique(by)) == 1 & !is.na(by[1])) {
      stop("Variable 'by' only has one unique value")
    }
    if (length(by) > 1) {
      if (length(unique(by)) > 2 & sum(is.na(by)) == 0) {
        stop("Variable 'by' has more than two unique values.")
      }
      if (length(by) != length(var1)) {
        stop("Variable 'by' is not of equal length to data vector")
      }
      # stratify var1 by by variable
      var1<-var1[!is.na(by)]
      by<-by[!is.na(by)]
      by1 <- sort(unique(by), decreasing = TRUE)[1]
      by2 <- sort(unique(by), decreasing = TRUE)[2]
      var2 <- var1[by == by1]
      var1 <- var1[by == by2]
      # check for equal length vectors in paired test, using by
      if (matched && length(var1) != length(var2)){
        stop("Cannot perform matched t-test on variable of unequal length")
      }
    }
    digits <- 3 + more.digits
    
    cl <- (1 + conf.level)/2
    # adjust values if the geometric means are to be compared
    if (geom) {
      var1 <- log(var1)
      var2 <- log(var2)
      if (null.hypoth == 0) {
        null.hypoth = 1
        warning("Geometric mean of zero not allowable: alternative hypothesis default changed to 1")
      }
      null.hypoth <- log(null.hypoth)
    }
    var1_orig <- var1
    var2_orig <- var2
    if (matched) {
      to.include <- (1 - as.numeric(is.na(var1))) * (1 - 
                                                       as.numeric(is.na(var2)))
      var1[to.include == 0] <- NA
      var2[to.include == 0] <- NA
    }

    
    # Case where by is not entered
    if (length(by) == 1 & is.na(by[1])) {
      # var2 is not entered
      if (length(var2) == 1 & is.na(var2[1])) {
        # here we do one sample t test with var1 using built in t.test function
        route <- stats::t.test(var1, alternative = alternative, 
                        var.equal = var.eq, conf.level = conf.level, 
                        mu = null.hypoth)
        mn <- route$estimate
        stdev <- stats::sd(var1, na.rm = TRUE)
        dfr <- as.numeric(route$parameter[1])
        ster <- stdev/sqrt(dfr + 1)
        tstat <- route$statistic
        talpha <- stats::qt(cl, dfr)
        # do two-sided CI, even if test is one-sided
        lci <- mn - talpha * ster
        hci <- mn + talpha * ster
        mn <- as.numeric(format(mn, digits = digits))
        stdev <- as.numeric(format(stdev, digits = digits))
        pval <- as.numeric(format(route$p.value, digits = max(digits, 
                                                              digits + 3)))
        tstat <- as.numeric(format(tstat, digits = max(digits + 
                                                         1, 3)))
        ster <- as.numeric(format(ster, digits = digits))
        lci <- as.numeric(format(lci, digits = digits))
        hci <- as.numeric(format(hci, digits = digits))
      
        main <- matrix(c(myargs[1], length(var1), sum(is.na(var1)), 
                         mn, ster, stdev, paste("[", lci, ", ", hci, 
                                                "]", sep = "")), ncol = 7)
        main <- data.frame(main)
        names(main) <- c("Variable", "Obs", "Missing", 
                         "Mean", "Std. Err.", "Std. Dev.", paste(conf.level * 
                                                                   100, "% CI", sep = ""))
        row.names(main) <- c("")
        main2 <- matrix(c(myargs[1], length(var1), sum(is.na(var1)), 
                          as.numeric(format(exp(mn), digits = digits)), 
                          paste("[", as.numeric(format(exp(lci), digits = digits)), 
                                ", ", as.numeric(format(exp(hci), digits = digits)), 
                                "]", sep = "")), ncol = 5)
        main2 <- data.frame(main2)
        names(main2) <- c("Variable", "Obs", "Missing", 
                          "Geom. Mean", paste(conf.level * 100, "% CI", 
                                              sep = ""))
        row.names(main2) <- c("")
      }
      if (length(var2) > 1) {
        route <- stats::t.test(var1, var2, alternative = alternative, 
                        var.equal = var.eq, conf.level = conf.level, 
                        paired = matched, mu = null.hypoth)
        mns <- c(mean(var1_orig, na.rm = TRUE), mean(var2_orig, 
                                                na.rm = TRUE), as.numeric(route$estimate[1]))
        if (!matched) {
          mns[3] <- mns[3] - as.numeric(route$estimate[2])
        }
        stdev <- c(stats::sd(var1_orig, na.rm = TRUE), stats::sd(var2_orig, na.rm = TRUE), 
                   NA)
        if (matched) {
          stdev[3] <- stats::sd((var2 - var1), na.rm = TRUE)
        }
        ns <- c(length(var1), length(var2), length(var1))
        if (!matched) {
          ns[3] <- length(var1) + length(var2)
        }
        msg <- c(sum(is.na(var1_orig)), sum(is.na(var2_orig)), 
                 sum(is.na(var1_orig)) + sum(is.na(var2_orig)))
        if (matched) {
          msg[3] <- sum(is.na(var1 * var2))
        }
        dfr <- as.numeric(c((ns[1:2] - msg[1:2]), route$parameter[1]))
        ster <- stdev/sqrt(dfr)
        ster[3] <- abs((mean(var2, na.rm = TRUE) - mean(var1, 
                                                        na.rm = TRUE))/route$statistic)
        tstat <- route$statistic
        talpha <- c(stats::qt(cl, dfr[1] - 1), stats::qt(cl, dfr[2] - 
                                             1), stats::qt(cl, dfr[3]))
        pval <- as.numeric(format(route$p.value, digits = max(digits, 
                                                              digits + 3)))
        tstat <- as.numeric(format(tstat, digits = max(digits + 
                                                         1, 3)))
        lci <- as.numeric(format(c(mns[1:2] - talpha[1:2] * 
                                     ster[1:2], route$conf.int[1]), digits = digits))
        hci <- as.numeric(format(c(mns[1:2] + talpha[1:2] * 
                                     ster[1:2], route$conf.int[2]), digits = digits))
        mns <- as.numeric(format(mns, digits = digits))
        stdev <- c(as.numeric(format(stdev[1:2], digits = digits)), 
                   NA)
        if (matched == TRUE) {
          stdev[3] <- as.numeric(format(ster[3] * sqrt(dfr[3]), 
                                        digits = digits))
        }
        ster <- as.numeric(format(ster, digits = digits))
        a <- 1
        b <- 2
        main <- matrix(c(myargs[a], ns[1], msg[1], mns[1], 
                         ster[1], stdev[1], paste("[", lci[1], ", ", 
                                                  hci[1], "]", sep = ""), myargs[b], ns[2], 
                         msg[2], mns[2], ster[2], stdev[2], paste("[", 
                                                                  lci[2], ", ", hci[2], "]", sep = ""), "Difference", 
                         ns[3], msg[3], mns[3], ster[3], stdev[3], paste("[", 
                                                                         lci[3], ", ", hci[3], "]", sep = "")), ncol = 7, 
                       byrow = TRUE)
        main <- data.frame(main)
        names(main) <- c("Group", "Obs", "Missing", "Mean", 
                         "Std. Err.", "Std. Dev.", paste(conf.level * 
                                                           100, "% CI", sep = ""))
        row.names(main) <- c("", " ", "  ")
        dfr <- as.numeric(format(dfr[3], digits = digits))
        main2 <- matrix(c(myargs[a], ns[1], msg[1], as.numeric(format(exp(mns[1]), 
                                                                      digits = digits)), paste("[", as.numeric(format(exp(lci[1]), 
                                                                                                                      digits = digits)), ", ", as.numeric(format(exp(hci[1]), 
                                                                                                                                                                 digits = digits)), "]", sep = ""), myargs[b], 
                          ns[2], msg[2], as.numeric(format(exp(mns[2]), 
                                                           digits = digits)), paste("[", as.numeric(format(exp(lci[2]), 
                                                                                                           digits = digits)), ", ", as.numeric(format(exp(hci[2]), 
                                                                                                                                                      digits = digits)), "]", sep = ""), "Ratio", 
                          ns[3], msg[3], as.numeric(format(exp(mns[3]), 
                                                           digits = digits)), paste("[", as.numeric(format(exp(lci[3]), 
                                                                                                           digits = digits)), ", ", as.numeric(format(exp(hci[3]), 
                                                                                                                                                      digits = digits)), "]", sep = "")), ncol = 5, 
                        byrow = TRUE)
        main2 <- data.frame(main2)
        names(main2) <- c("Group", "Obs", "Missing", 
                          "Geom. Mean", paste(conf.level * 100, "% CI", 
                                              sep = ""))
        row.names(main2) <- c("", " ", "  ")
      }
    }
    if (length(by) > 1) {
      if (length(var2) > 1) {
        route <- stats::t.test(var1, var2, alternative = alternative, 
                        var.equal = var.eq, conf.level = conf.level, 
                        paired = matched, mu = null.hypoth)
        mns <- c(mean(var1, na.rm = TRUE), mean(var2, 
                                                na.rm = TRUE), as.numeric(route$estimate[1]))
        if (!matched) {
          mns[3] <- mns[3] - as.numeric(route$estimate[2])
        }
        stdev <- c(stats::sd(var1, na.rm = TRUE), stats::sd(var2, na.rm = TRUE), 
                   NA)
        if (matched) {
          stdev[3] <- stats::sd((var2 - var1), na.rm = TRUE)
        }
        ns <- c(length(var1), length(var2), length(var1))
        if (!matched) {
          ns[3] <- length(var1) + length(var2)
        }
        msg <- c(sum(is.na(var1)), sum(is.na(var2)), 
                 sum(is.na(var1)) + sum(is.na(var2)))
        if (matched) {
          msg[3] <- sum(is.na(var1 * var2))
        }
        dfr <- as.numeric(c((ns[1:2] - msg[1:2]), route$parameter[1]))
        ster <- stdev/sqrt(dfr)
        ster[3] <- abs((mean(var2, na.rm = TRUE) - mean(var1, 
                                                        na.rm = TRUE))/route$statistic)
        tstat <- route$statistic
        talpha <- c(stats::qt(cl, dfr[1] - 1), stats::qt(cl, dfr[2] - 
                                             1), stats::qt(cl, dfr[3]))
        pval <- as.numeric(format(route$p.value, digits = max(digits, 
                                                              digits + 3)))
        tstat <- as.numeric(format(tstat, digits = max(digits + 
                                                         1, 3)))
        lci <- as.numeric(format(c(mns[1:2] - talpha[1:2] * 
                                     ster[1:2], route$conf.int[1]), digits = digits))
        hci <- as.numeric(format(c(mns[1:2] + talpha[1:2] * 
                                     ster[1:2], route$conf.int[2]), digits = digits))
        mns <- as.numeric(format(mns, digits = digits))
        stdev <- c(as.numeric(format(stdev[1:2], digits = digits)), 
                   NA)
        if (matched) {
          stdev[3] <- as.numeric(format(ster[3] * sqrt(dfr[3]), 
                                        digits = digits))
        }
        ster <- as.numeric(format(ster, digits = digits))
        a <- 1
        b <- 2
        main <- matrix(c(paste(myargs[3], " = ", byt[a], 
                               sep = ""), ns[1], msg[1], mns[1], ster[1], 
                         stdev[1], paste("[", lci[1], ", ", hci[1], 
                                         "]", sep = ""), paste(myargs[3], " = ", byt[b], 
                                                               sep = ""), ns[2], msg[2], mns[2], ster[2], 
                         stdev[2], paste("[", lci[2], ", ", hci[2], 
                                         "]", sep = ""), "Difference", ns[3], msg[3], 
                         mns[3], ster[3], stdev[3], paste("[", lci[3], 
                                                          ", ", hci[3], "]", sep = "")), ncol = 7, 
                       byrow = TRUE)
        main <- data.frame(main)
        names(main) <- c("Group", "Obs", "Missing", "Mean", 
                         "Std. Err.", "Std. Dev.", paste(conf.level * 
                                                           100, "% CI", sep = ""))
        row.names(main) <- c("", " ", "  ")
        main2 <- matrix(c(paste(myargs[3], " = ", byt[a], 
                                sep = ""), ns[1], msg[1], as.numeric(format(exp(mns[1]), 
                                                                            digits = digits)), paste("[", as.numeric(format(exp(lci[1]), 
                                                                                                                            digits = digits)), ", ", as.numeric(format(exp(hci[1]), 
                                                                                                                                                                       digits = digits)), "]", sep = ""), paste(myargs[3], 
                                                                                                                                                                                                                " = ", byt[b], sep = ""), ns[2], msg[2], as.numeric(format(exp(mns[2]), 
                                                                                                                                                                                                                                                                           digits = digits)), paste("[", as.numeric(format(exp(lci[2]), 
                                                                                                                                                                                                                                                                                                                           digits = digits)), ", ", as.numeric(format(exp(hci[2]), 
                                                                                                                                                                                                                                                                                                                                                                      digits = digits)), "]", sep = ""), "Ratio", 
                          ns[3], msg[3], as.numeric(format(exp(mns[3]), 
                                                           digits = digits)), paste("[", as.numeric(format(exp(lci[3]), 
                                                                                                           digits = digits)), ", ", as.numeric(format(exp(hci[3]), 
                                                                                                                                                      digits = digits)), "]", sep = "")), ncol = 5, 
                        byrow = TRUE)
        main2 <- data.frame(main2)
        names(main2) <- c("Group", "Obs", "Missing", 
                          "Geom. Mean", paste(conf.level * 100, "% CI", 
                                              sep = ""))
        row.names(main2) <- c("", " ", "  ")
        dfr <- as.numeric(format(dfr[3], digits = digits))
      }
    }
    par <- c(geom = geom, null.hypoth = null.hypoth, alternative = alternative, 
             var.eq = var.eq, conf.level = conf.level, matched = matched, 
             digits = digits)
    invisible(list(tab = main, df = dfr, p = pval, tstat = tstat, 
                   var1 = var1, var2 = var2, by = by, par = par, geo = main2))
  }
  
  
  myargs <- c(deparse(substitute(var1)), deparse(substitute(var2)), 
              deparse(substitute(by)))
  ttest.obj <- ttest.do(var1 = var1, var2 = var2, 
                        geom = geom, 
                        by = by, null.hypoth = null.hypoth, alternative = alternative, 
                        var.eq = var.eq, conf.level = conf.level, 
                        matched = matched, more.digits = more.digits, 
                        myargs = myargs)
  
  ttest.obj$call <- match.call()
  class(ttest.obj) <- "ttest"
  return(ttest.obj)
}
