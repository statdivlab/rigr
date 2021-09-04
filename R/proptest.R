#' Test of proportions with improved layout
#' 
#' Produces table of relevant descriptive statistics and inference for either
#' one- or two-sample test of proportions. This test can be approximate or exact and has
#' the option to use a continuity correction. 
#' 
#' Missing values must be given by \code{"NA"}s to be recognized as missing values. Any
#' call to \code{proptest()} is run by \code{proptest.default()}, with user specified
#' values in place of defaults in the appropriate places.
#' 
#' @aliases proptest proptest.do plot.proptest print.proptest proptest.default
#' 
#' @param var1 a (non-empty) numeric vector of
#' data values.
#' @param var2 an optional (non-empty) numeric
#' vector of data.
#' @param by a variable of equal length to
#' that of \code{var1} with two outcomes. This will be used to define strata
#' for a prop test on \code{var1}.
#' @param exact If true, performs a
#' test of equality of proportions with Exact Binomial based confidence
#' intervals.
#' @param null.hypoth a number specifying the
#' null hypothesis for the mean (or difference in means if performing a
#' two-sample test). Defaults to zero.
#' @param alternative a string: one of
#' \code{"less"}, \code{"two.sided"}, or \code{"greater"} specifying the form
#' of the test. Defaults to a two-sided test.
#' @param conf.level confidence level of the
#' test. Defaults to 95/100.
#' @param more.digits a numeric value
#' specifying whether or not to display more or fewer digits in the output.
#' Non-integers are automatically rounded down. 
#' 
#' @return Prints a summary of the data and the
#' corresponding proportions test. \item{Variable}{the variable
#' name supplied to the prop test function} 
#' 
#' \item{Group}{the group name: either
#' the variable names supplied to the function or the names of the strata if
#' the variable \code{by} was specified.} 
#' \item{Obs}{Number of observations of
#' each variable: includes missing values.} 
#' \item{Missing}{number of missing
#' values in each data vector.} 
#' \item{Mean}{the sample mean of each data
#' vector; also, the estimated difference in means in a two-sample test.}
#' \item{Std.Err.}{the estimated standard error of the mean and of the
#' difference in the two-sample test.} 
#' \item{CI}{a confidence interval for the means, and
#' for the difference in the two-sample test. This is at the confidence level
#' specified in the argument and is a two-sided confidence interval, regardless of the 
#' alternatuive hypothesis. If \code{prop} and/or \code{exact} are
#' \code{TRUE}, also returns the appropriate confidence interval for the test
#' of equality of proportions.}
#' \item{Null hypothesis}{a statement of the null
#' hypothesis.} 
#' \item{Alternative hypothesis}{a statement of the alternative
#' hypothesis.}
#' \item{t}{value of the Z-statistic, if applicable} 
#' \item{df}{the degrees of
#' freedom for the test.} 
#' \item{Pr}{a p-value for inference on the
#' corresponding hypothesis test.}
#' 
#' @seealso \code{\link[stats]{prop.test}}
#' @examples
#' 
#' #- Read in data set -#
#' data(psa)
#' attach(psa)
#' 
#' #- Define new binary variable as indicator -#
#' #- of whether or not bss was worst possible -#
#' bssworst <- bss
#' bssworst[bss == 1] <- 0
#' bssworst[bss == 2] <- 0
#' bssworst[bss == 3] <- 1
#' 
#' 
#' #- Perform test comparing proportion in remission -#
#' #- between bss strata, using dummy to make inrem a binary numeric -#
#' proptest(dummy(inrem), by = bssworst)
#' 
#' @export proptest
proptest<-function (var1, var2 = NA, by = NA, exact = FALSE, null.hypoth = 0.5, alternative = "two.sided", 
                 conf.level = 0.95, more.digits = 0) 
{
  proptest.do <- function(var1, var2 = NA, by = NA, 
                       exact = FALSE, null.hypoth = 0.5, alternative = "two.sided", 
                       conf.level = 0.95, more.digits = 0, 
                       myargs, ...) {
    # can only do var1 vs var2 or var1 by
    if (length(var2) > 1 & length(by) > 1) {
      stop("Please specify only one of the variables 'by' or 'var2'")
    }
    # check that alternative is one of "less", "two.sided", or "greater"
    if (!(alternative %in% c("less", "two.sided", "greater"))) {
      stop("'alternative' must be either 'less', 'two.sided', or 'greater'")
    }
    # if (!(is.scalar(null.hypoth))){
    #   stop("Null must be a scalar.")
    # }
    ### to do - make sure conf.level is scalar! and null is between 0 and 1 and data are 0-1, by is same length as var1,
    ### can't do exact two-prop test, can't specify null for two-sample test
    if (!((length(conf.level) == 1L) && is.finite(conf.level) && 
          (conf.level > 0) && (conf.level < 1))){
      stop("'conf.level' must be a single number between 0 and 1")
    } 
    # get length of stratafication variable (byt)
    byt <- sort(unique(by),decreasing=FALSE)
    byt <- byt[!is.na(byt)]
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
    }
    digits <- 3 + more.digits
    
    cl <- (1 + conf.level)/2
    # Case where by is not entered
    if (length(by) == 1 & is.na(by[1])) {
      # var2 is not entered
      if (length(var2) == 1 & is.na(var2[1])) {
        ns <- c(length(var1))
        msg <- sum(is.na(var1))
        var1 <- var1[!is.na(var1)]
        # here we do one sample prop test with var1 
        est1 <- mean(var1)
        se <- NULL
        cil <- NULL
        cih <- NULL
        zstat <- NULL
        pval <- NULL
        est1 <- as.numeric(format(est1), digits = digits)
        
        if (exact) {
          binom <- stats::binom.test(x = sum(var1), n = length(var1), p = null.hypoth, 
                              alternative = alternative, conf.level = conf.level)
          se <- sqrt(est1*(1-est1)/length(var1))
          cil <- as.numeric(format(min(binom$conf.int), 
                                   digits = digits))
          cih <- as.numeric(format(max(binom$conf.int), 
                                   digits = digits))
          se <- as.numeric(format(se, digits = digits))
          pval <- as.numeric(format(binom$p.value, 
                                    digits = digits))
        } else {
          chisq <- stats::prop.test(x = sum(var1), n = length(var1), conf.level = conf.level, p = null.hypoth,
                             alternative = alternative, correct = FALSE)
          zstat <- as.numeric(format(sqrt(chisq$statistic), 
                                         digits = digits))
          pval <- as.numeric(format(chisq$p.value, 
                                      digits = digits))
          se <- sqrt(est1*(1-est1)/length(var1))
          cil <- est1 - stats::qnorm(cl) * se
          cih <- est1 + stats::qnorm(cl) * se
          cil <- as.numeric(format(cil, digits = digits))
          cih <- as.numeric(format(cih, digits = digits))
          se <- as.numeric(format(se, digits = digits))
        }
        main <- matrix(c(myargs[1], ns[1], msg[1], 
                          est1, se, paste("[", cil, ", ", cih, 
                                                 "]", sep = "")), ncol = 6)
        main <- data.frame(main)
        names(main) <- c("Variable", "Obs", "Missing", 
                          "Estimate", "Std. Err.", paste(conf.level * 100, "% CI", sep = ""))
        row.names(main) <- c("")
      }
      # two-sample, not using by
      if (length(var2) > 1) {
        ns <- c(length(var1), length(var2), length(var1) + length(var2))
        msg <- c(sum(is.na(var1)), sum(is.na(var2)), sum(is.na(var1)) + sum(is.na(var2)))
        var1 <- var1[!is.na(var1)]
        var2 <- var2[!is.na(var2)]
        est1 <- mean(var1)
        est2 <- mean(var2)
        se <- NULL
        cil <- NULL
        cih <- NULL
        zstat <- NULL
        pval <- NULL
        est1 <- as.numeric(format(est1), digits = digits)
        est2 <- as.numeric(format(est2), digits = digits)
        est_diff <- est1 - est2
        est_diff <- as.numeric(format(est_diff), digits = digits)
        # no two-sample exact test
        chisq <- stats::prop.test(x = c(sum(var1), sum(var2)),
                           n = c(length(var1), length(var2)),
                           conf.level = conf.level, alternative = alternative, correct = FALSE)
        zstat <- as.numeric(format(sqrt(chisq$statistic), 
                                       digits = digits))
        pval <- as.numeric(format(chisq$p.value, 
                                    digits = digits))
        se <- c(sqrt(est1 * (1-est1)/length(var1)),
                sqrt(est2 * (1-est2)/length(var2)),
                sqrt((est1 * (1-est1))/length(var1) + (est2*(1-est2))/length(var2)))
        cil <- c(est1 - stats::qnorm(cl)*se[1],
                 est2 - stats::qnorm(cl)*se[2],
                 est1 - est2 - stats::qnorm(cl) * se[3])
        cih <- c(est1 + stats::qnorm(cl)*se[1],
                 est2 + stats::qnorm(cl)*se[2],
                 est1 - est2 + stats::qnorm(cl) * se[3])
        cil <- as.numeric(format(cil, digits = digits))
        cih <- as.numeric(format(cih, digits = digits))
        se <- as.numeric(format(se, digits = digits))
        
        main <- matrix(c(myargs[1], ns[1], msg[1], est1, 
                           se[1], paste("[", cil[1], ", ", cih[1], "]", sep = ""), 
                         myargs[2], ns[2], msg[2], 
                           est2, se[2],  paste("[", cil[2], ", ", cih[2], "]", sep = ""), 
                         "Difference", ns[3], msg[3], 
                            est_diff, se[3], paste("[", cil[3], ", ", cih[3], "]", sep = "")),
                       ncol = 6, byrow = TRUE)
        main <- data.frame(main)
        names(main) <- c("Group", "Obs", "Missing", 
                          "Mean", "Std. Err.", paste(conf.level * 100, "% CI", sep = ""))
        row.names(main) <- c("", " ", "  ")
      }
    }
    # two-sample, using by
    if (length(by) > 1 && length(var2) > 1) {
      ns <- c(length(var1), length(var2), length(var1) + length(var2))
      msg <- c(sum(is.na(var1)), sum(is.na(var2)), sum(is.na(var1)) + sum(is.na(var2)))
      var1 <- var1[!is.na(var1)]
      var2 <- var2[!is.na(var2)]
      est1 <- mean(var1)
      est2 <- mean(var2)
      se <- NULL
      cil <- NULL
      cih <- NULL
      zstat <- NULL
      pval <- NULL
      est1 <- as.numeric(format(est1), digits = digits)
      est2 <- as.numeric(format(est2), digits = digits)
      est_diff <- est1 - est2
      est_diff <- as.numeric(format(est_diff), digits = digits)
      chisq <- stats::prop.test(x = c(sum(var1), sum(var2)),
                         n = c(length(var1), length(var2)),
                         conf.level = conf.level, correct = FALSE,
                         alternative = alternative)
      
      zstat <- as.numeric(format(sqrt(chisq$statistic), 
                                 digits = digits))
      pval <- as.numeric(format(chisq$p.value, 
                                digits = digits))
      se <- c(sqrt(est1 * (1-est1)/length(var1)),
              sqrt(est2 * (1-est2)/length(var2)),
              sqrt((est1 * (1-est1))/length(var1) + (est2*(1-est2))/length(var2)))
      cil <- c(est1 - stats::qnorm(cl)*se[1],
               est2 - stats::qnorm(cl)*se[2],
               est1 - est2 - stats::qnorm(cl) * se[3])
      cih <- c(est1 + stats::qnorm(cl)*se[1],
               est2 + stats::qnorm(cl)*se[2],
               est1 - est2 + stats::qnorm(cl) * se[3])
      cil <- as.numeric(format(cil, digits = digits))
      cih <- as.numeric(format(cih, digits = digits))
      se <- as.numeric(format(se, digits = digits))
      
      main <- matrix(c(paste(myargs[3], " = ", byt[1], 
                              sep = ""), ns[1], msg[1], est1, se[1], 
                        paste("[", cil[1], ", ", cih[1], 
                                        "]", sep = ""), 
                       paste(myargs[3], " = ", byt[2], sep = ""), ns[2], msg[2], est2, 
                        se[2], paste("[", cil[2], ", ", 
                                                 cih[2], "]", sep = ""), 
                      "Difference", 
                        ns[3],
                        msg[3],
                        est_diff,
                        se[3],
                        paste("[", cil[3], ", ", cih[3], "]", sep = "")), 
                      ncol = 6, byrow = TRUE)
      main <- data.frame(main)
      names(main) <- c("Group", "Obs", "Missing", 
                        "Mean", "Std. Err.", paste(conf.level * 
                                                                  100, "% CI", sep = ""))
      row.names(main) <- c("", " ", "  ")
      
    }
    par <- c(null.hypoth = null.hypoth, alternative = alternative, 
             conf.level = conf.level, exact = exact, digits = digits)
    invisible(list(tab = main, zstat = zstat, pval = pval, 
                   var1 = var1, var2 = var2, by = by, par = par))
  }
    
    
    myargs <- c(deparse(substitute(var1)), deparse(substitute(var2)), 
                deparse(substitute(by)))
    uby <- unique(by)
    
    if (is.na(var2[1]) & length(var2) == 1) {
      if (is.na(by[1]) & length(by) == 1) {
        strat <- rep(1, length(var1))
      }
      if (length(by) > 1) {
        strat <- rep(1, length(var1))
      }
    }
    if (length(var2) > 1) {
      strat1 <- rep(1, length(var1))
      strat2 <- rep(1, length(var2))
    }
    
    
    if (is.na(var2[1]) & length(var2) == 1) {
      if (is.na(by[1]) & length(by) == 1) {
        ustrat <- unique(strat)
        for (t in 1:length(ustrat)) {
          x <- subset(var1, strat == ustrat[t])
          proptest.obj <- proptest.do(var1 = x, var2 = var2, 
                                      by = by, exact = exact,
                                      null.hypoth = null.hypoth, alternative = alternative, 
                                      conf.level = conf.level, 
                                      more.digits = more.digits, 
                                      myargs = myargs)
          
        }
      }
      if (length(by) > 1) {
        ustrat <- unique(strat)
        for (t in 1:length(ustrat)) {
          x <- subset(var1, strat == ustrat[t])
          cby <- subset(by, strat == ustrat[t])
          proptest.obj <- proptest.do(var1 = x, var2 = var2, 
                                by = cby, exact = exact,
                                null.hypoth = 0, alternative = alternative, 
                                conf.level = conf.level, 
                                more.digits = more.digits, 
                                myargs = myargs)
        }
      }
    }
    if (length(var2) > 1) {
      ustrat <- unique(c(strat1, strat2))
      for (t in 1:length(ustrat)) {
        x <- subset(var1, strat1 == ustrat[t])
        y <- subset(var2, strat2 == ustrat[t])
        proptest.obj <- proptest.do(var1 = x, var2 = y, 
                              by = by,
                              exact = exact,
                              null.hypoth = 0, 
                              alternative = alternative, 
                              conf.level = conf.level, 
                              more.digits = more.digits, 
                              myargs = myargs)
      }
    }
    proptest.obj$call <- match.call()
    class(proptest.obj) <- "proptest"
    return(proptest.obj)
}
