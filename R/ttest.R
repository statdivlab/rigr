ttest<-function (var1, var2 = NA, by = NA, strat = NULL, geom = FALSE, 
          prop = FALSE, exact = FALSE, null.hypoth = 0, test.type = "two.sided", 
          var.eq = FALSE, conf.level = 0.95, matched = FALSE, more.digits = 0) 
{
  ttest.do <- function(var1, var2 = NA, by = NA, geom = FALSE, 
                       prop = FALSE, exact = FALSE, null.hypoth = 0, test.type = "two.sided", 
                       var.eq = FALSE, conf.level = 0.95, matched = FALSE, more.digits = 0, 
                       myargs, ...) {
    var1 <- var1
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
    # See whether requested test presumes equal variance
    if (var.eq == TRUE) {
      robust <- FALSE
    }
    if (var.eq == FALSE) {
      robust <- TRUE
    }
    # checks to make sure it is proportion if exact binomial conf int is requested
    if (exact == TRUE & prop == FALSE) {
      stop("Exact binomial confidence intervals cannot be computed for anything but a proportion.")
    }
    # make sure var.eq is either true or false
    if (!is.logical(var.eq)) {
      stop("Please specify a logical value for variable 'var.eq'")
    }
    # get length of stratafication variable (byt)
    byt <- sort(unique(by),decreasing=FALSE)
    byt <- byt[!is.na(byt)]
    # make sure var1 and var2 are the same length if a paired t test is requested
    if (matched == TRUE & (length(var2) > 1)) {
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
    }
    digits <- 3 + more.digits
    
    # vareq is made into var.eq, basically
    vareq <- as.logical(1 - robust)
    cl <- (1 + conf.level)/2
    if (matched == TRUE) {
      to.include <- (1 - as.numeric(is.na(var1))) * (1 - 
                                                       as.numeric(is.na(var2)))
      var1[to.include == 0] <- NA
      var2[to.include == 0] <- NA
    }
    # adjust values if the geometric means are to be compared
    if (geom == TRUE) {
      var1 <- log(var1)
      var2 <- log(var2)
      if (null.hypoth == 0) {
        null.hypoth = 1
        warning("Geometric mean of zero not allowable: alternative hypothesis default changed to 1")
      }
      null.hypoth <- log(null.hypoth)
    }
    
    # Case where by is not entered
    if (length(by) == 1 & is.na(by[1])) {
      # var2 is not entered
      if (length(var2) == 1 & is.na(var2[1])) {
        # here we do one sample t test with var1 using built in t.test function
        route <- t.test(var1, alternative = test.type, 
                        var.equal = vareq, conf.level = conf.level, 
                        mu = null.hypoth)
        mn <- route$estimate
        stdev <- sd(var1, na.rm = TRUE)
        dfr <- as.numeric(route$parameter[1])
        ster <- stdev/sqrt(dfr + 1)
        tstat <- route$statistic
        talpha <- qt(cl, dfr)
        #### WHY NOT USE BUILT IN CONF.INT FROM T.TEST HERE?
        lci <- mn - talpha * ster
        hci <- mn + talpha * ster
        mn <- as.numeric(format(mn, digits = digits))
        stdev <- as.numeric(format(stdev, digits = digits))
        ### HERE WE USE BUILT IN P VALUE AND T STATISTIC
        pval <- as.numeric(format(route$p.value, digits = max(digits, 
                                                              digits + 3)))
        tstat <- as.numeric(format(tstat, digits = max(digits + 
                                                         1, 3)))
        ster <- as.numeric(format(ster, digits = digits))
        lci <- as.numeric(format(lci, digits = digits))
        hci <- as.numeric(format(hci, digits = digits))
        propSter <- NULL
        propCIL <- NULL
        propCIH <- NULL
        chisqStat <- NULL
        chisqP <- NULL
        chisqdf <- NULL
        if (prop) {
          if (exact) {
            if (abs(route$estimate) < 1) {
              binom <- binom.test(x = round(abs(route$estimate * 
                                                   length(var1))), n = length(var1), p = null.hypoth + 
                                    1e-10, alternative = test.type, conf.level = cl)
              propCIL <- as.numeric(format(min(binom$conf.int), 
                                           digits = digits))
              propCIH <- as.numeric(format(max(binom$conf.int), 
                                           digits = digits))
            }
            else {
              binom <- binom.test(x = round(abs(route$estimate)), 
                        #####          n = length(var1), p = null.hypoth + 1e-10, 
                                  alternative = test.type, conf.level = cl)
              propCIL <- as.numeric(format(min(binom$conf.int) * 
                                             length(var1), digits = digits))
              propCIH <- as.numeric(format(max(binom$conf.int) * 
                                             length(var1), digits = digits))
            }
            chisqStat <- as.numeric(format(binom$statistic, 
                                           digits = digits))
            chisqdf <- as.numeric(format(binom$parameter, 
                                         digits = digits))
            chisqP <- as.numeric(format(binom$p.value, 
                                        digits = digits))
          }
          else {
            if (abs(route$estimate) < 1) {
              chisq <- prop.test(x = round(abs(route$estimate * 
                                                 length(var1))), n = length(var1), conf.level = cl, 
                                 correct = FALSE)
            }
            else {
              chisq <- prop.test(x = round(abs(route$estimate)), 
                                 n = length(var1), conf.level = cl, correct = FALSE)
            }
            chisqStat <- as.numeric(format(chisq$statistic, 
                                           digits = digits))
            chisqdf <- as.numeric(format(chisq$parameter, 
                                         digits = digits))
            chisqP <- as.numeric(format(chisq$p.value, 
                                        digits = digits))
            propSter <- sd(var1, na.rm = TRUE) * sqrt(1/length(var1))
            propCIL <- route$estimate - qnorm(cl) * propSter
            propCIH <- route$estimate + qnorm(cl) * propSter
            propCIL <- as.numeric(format(propCIL, digits = digits))
            propCIH <- as.numeric(format(propCIH, digits = digits))
            propSter <- as.numeric(format(propSter, digits = digits))
          }
        }
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
        main3 <- matrix(c(myargs[1], length(var1), sum(is.na(var1)), 
                          mn, ster, stdev, paste("[", lci, ", ", hci, 
                                                 "]", sep = ""), paste("[", propCIL, ", ", 
                                                                       propCIH, "]", sep = "")), ncol = 8)
        main3 <- data.frame(main3)
        names(main3) <- c("Variable", "Obs", "Missing", 
                          "Mean", "Std. Err.", "Std. Dev.", paste(conf.level * 
                                                                    100, "% CI", sep = ""), paste(conf.level * 
                                                                                                    100, "% CI", " [Wald]", sep = ""))
        row.names(main3) <- c("")
        main4 <- matrix(c(myargs[1], length(var1), sum(is.na(var1)), 
                          mn, ster, stdev, paste("[", lci, ", ", hci, 
                                                 "]", sep = ""), paste("[", propCIL, ", ", 
                                                                       propCIH, "]", sep = "")), ncol = 8)
        main4 <- data.frame(main4)
        names(main4) <- c("Variable", "Obs", "Missing", 
                          "Mean", "Std. Err.", "Std. Dev.", paste(conf.level * 
                                                                    100, "% CI", sep = ""), paste(conf.level * 
                                                                                                    100, "% CI", " [Exact Binomial]", sep = ""))
        row.names(main4) <- c("")
      }
      if (length(var2) > 1) {
        route <- t.test(var1, var2, alternative = test.type, 
                        var.equal = vareq, conf.level = conf.level, 
                        paired = matched, mu = null.hypoth)
        mns <- c(mean(var1, na.rm = TRUE), mean(var2, 
                                                na.rm = TRUE), as.numeric(route$estimate[1]))
        if (matched == FALSE) {
          mns[3] <- mns[3] - as.numeric(route$estimate[2])
        }
        stdev <- c(sd(var1, na.rm = TRUE), sd(var2, na.rm = TRUE), 
                   NA)
        if (matched == TRUE) {
          stdev[3] <- sd((var2 - var1), na.rm = TRUE)
        }
        ns <- c(length(var1), length(var2), length(var1))
        if (matched == FALSE) {
          ns[3] <- length(var1) + length(var2)
        }
        msg <- c(sum(is.na(var1)), sum(is.na(var2)), 
                 sum(is.na(var1)) + sum(is.na(var2)))
        if (matched == TRUE) {
          msg[3] <- sum(is.na(var1 * var2))
        }
        dfr <- as.numeric(c((ns[1:2] - msg[1:2]), route$parameter[1]))
        ster <- stdev/sqrt(dfr)
        ster[3] <- abs((mean(var2, na.rm = TRUE) - mean(var1, 
                                                        na.rm = TRUE))/route$statistic)
        tstat <- route$statistic
        talpha <- c(qt(cl, dfr[1] - 1), qt(cl, dfr[2] - 
                                             1), qt(cl, dfr[3]))
        propSter <- NULL
        propCIL <- NULL
        propCIH <- NULL
        chisqStat <- NULL
        chisqP <- NULL
        chisqdf <- NULL
        if (prop) {
          if (exact) {
            if (abs((route$estimate[1] - route$estimate[2])) < 
                1) {
              binom <- binom.test(x = round(abs((route$estimate[1] - 
                                                   route$estimate[2])) * (length(var1) + 
                                                                            length(var2))), n = length(var1) + length(var2), 
                                  p = null.hypoth + 1e-10, alternative = test.type, 
                                  conf.level = cl)
              propCIL <- as.numeric(format(min(binom$conf.int), 
                                           digits = digits))
              propCIH <- as.numeric(format(max(binom$conf.int), 
                                           digits = digits))
            }
            else {
              binom <- binom.test(x = round(abs((route$estimate[1] - 
                                                   route$estimate[2]))), n = length(var1) + 
                                    length(var2), p = null.hypoth + 1e-10, 
                                  alternative = test.type, conf.level = cl)
              propCIL <- as.numeric(format(min(binom$conf.int) * 
                                             (length(var1) + length(var2)), digits = digits))
              propCIH <- as.numeric(format(max(binom$conf.int) * 
                                             (length(var1) + length(var2)), digits = digits))
            }
            chisqStat <- as.numeric(format(binom$statistic, 
                                           digits = digits))
            chisqdf <- as.numeric(format(binom$parameter, 
                                         digits = digits))
            chisqP <- as.numeric(format(binom$p.value, 
                                        digits = digits))
          }
          else {
            if (abs((route$estimate[1] - route$estimate[2])) < 
                1) {
              chisq <- prop.test(x = round(abs((route$estimate[1] - 
                                                  route$estimate[2])) * (length(var1) + 
                                                                           length(var2))), n = length(var1) + length(var2), 
                                 conf.level = cl, correct = FALSE)
            }
            else {
              chisq <- prop.test(x = round(abs((route$estimate[1] - 
                                                  route$estimate[2]))), n = length(var1) + 
                                   length(var2), conf.level = cl, correct = FALSE)
            }
            chisqStat <- as.numeric(format(chisq$statistic, 
                                           digits = digits))
            chisqdf <- as.numeric(format(chisq$parameter, 
                                         digits = digits))
            chisqP <- as.numeric(format(chisq$p.value, 
                                        digits = digits))
            propSter <- sd(var1, na.rm = TRUE) * sqrt(1/length(var1))
            propCIL <- route$estimate[1] - route$estimate[2] - 
              qnorm(cl) * propSter
            propCIH <- route$estimate[1] - route$estimate[2] + 
              qnorm(cl) * propSter
            propCIL <- as.numeric(format(propCIL, digits = digits))
            propCIH <- as.numeric(format(propCIH, digits = digits))
            propSter <- as.numeric(format(propSter, digits = digits))
          }
        }
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
        main3 <- matrix(c(myargs[a], ns[1], msg[1], mns[1], 
                          ster[1], stdev[1], paste("[", lci[1], ", ", 
                                                   hci[1], "]", sep = ""), " ", myargs[b], ns[2], 
                          msg[2], mns[2], ster[2], stdev[2], paste("[", 
                                                                   lci[2], ", ", hci[2], "]", sep = ""), " ", 
                          "Difference", ns[3], msg[3], mns[3], ster[3], 
                          stdev[3], paste("[", lci[3], ", ", hci[3], 
                                          "]", sep = ""), paste("[", propCIL, ", ", 
                                                                propCIH, "]", sep = "")), ncol = 8, byrow = TRUE)
        main3 <- data.frame(main3)
        names(main3) <- c("Group", "Obs", "Missing", 
                          "Mean", "Std. Err.", "Std. Dev.", paste(conf.level * 
                                                                    100, "% CI", sep = ""), paste(conf.level * 
                                                                                                    100, "% CI", " [Wald]", sep = ""))
        row.names(main3) <- c("", " ", "  ")
        main4 <- matrix(c(myargs[a], ns[1], msg[1], mns[1], 
                          ster[1], stdev[1], paste("[", lci[1], ", ", 
                                                   hci[1], "]", sep = ""), " ", myargs[b], ns[2], 
                          msg[2], mns[2], ster[2], stdev[2], paste("[", 
                                                                   lci[2], ", ", hci[2], "]", sep = ""), " ", 
                          "Difference", ns[3], msg[3], mns[3], ster[3], 
                          stdev[3], paste("[", lci[3], ", ", hci[3], 
                                          "]", sep = ""), paste("[", propCIL, ", ", 
                                                                propCIH, "]", sep = "")), ncol = 8, byrow = TRUE)
        main4 <- data.frame(main4)
        names(main4) <- c("Group", "Obs", "Missing", 
                          "Mean", "Std. Err.", "Std. Dev.", paste(conf.level * 
                                                                    100, "% CI", sep = ""), paste(conf.level * 
                                                                                                    100, "% CI", " [Exact Binomial]", sep = ""))
        row.names(main4) <- c("", " ", "  ")
      }
    }
    if (length(by) > 1) {
      if (length(var2) > 1) {
        route <- t.test(var1, var2, alternative = test.type, 
                        var.equal = vareq, conf.level = conf.level, 
                        paired = matched, mu = null.hypoth)
        mns <- c(mean(var1, na.rm = TRUE), mean(var2, 
                                                na.rm = TRUE), as.numeric(route$estimate[1]))
        if (matched == FALSE) {
          mns[3] <- mns[3] - as.numeric(route$estimate[2])
        }
        stdev <- c(sd(var1, na.rm = TRUE), sd(var2, na.rm = TRUE), 
                   NA)
        if (matched == TRUE) {
          stdev[3] <- sd((var2 - var1), na.rm = TRUE)
        }
        ns <- c(length(var1), length(var2), length(var1))
        if (matched == FALSE) {
          ns[3] <- length(var1) + length(var2)
        }
        msg <- c(sum(is.na(var1)), sum(is.na(var2)), 
                 sum(is.na(var1)) + sum(is.na(var2)))
        if (matched == TRUE) {
          msg[3] <- sum(is.na(var1 * var2))
        }
        dfr <- as.numeric(c((ns[1:2] - msg[1:2]), route$parameter[1]))
        ster <- stdev/sqrt(dfr)
        ster[3] <- abs((mean(var2, na.rm = TRUE) - mean(var1, 
                                                        na.rm = TRUE))/route$statistic)
        tstat <- route$statistic
        talpha <- c(qt(cl, dfr[1] - 1), qt(cl, dfr[2] - 
                                             1), qt(cl, dfr[3]))
        propSter <- NULL
        propCIL <- NULL
        propCIH <- NULL
        chisqStat <- NULL
        chisqP <- NULL
        chisqdf <- NULL
        if (prop) {
          if (exact) {
            if (abs((route$estimate[1] - route$estimate[2])) < 
                1) {
              binom <- binom.test(x = round(abs((route$estimate[1] - 
                                                   route$estimate[2])) * (length(var1) + 
                                                                            length(var2))), n = length(var1) + length(var2), 
                                  p = null.hypoth + 1e-10, alternative = test.type, 
                                  conf.level = cl)
              propCIL <- as.numeric(format(min(binom$conf.int), 
                                           digits = digits))
              propCIH <- as.numeric(format(max(binom$conf.int), 
                                           digits = digits))
            }
            else {
              binom <- binom.test(x = round(abs((route$estimate[1] - 
                                                   route$estimate[2]))), n = length(var1) + 
                                    length(var2), p = null.hypoth + 1e-10, 
                                  alternative = test.type, conf.level = cl)
              propCIL <- as.numeric(format(min(binom$conf.int) * 
                                             (length(var1) + length(var2)), digits = digits))
              propCIH <- as.numeric(format(max(binom$conf.int) * 
                                             (length(var1) + length(var2)), digits = digits))
            }
            chisqStat <- as.numeric(format(binom$statistic, 
                                           digits = digits))
            chisqdf <- as.numeric(format(binom$parameter, 
                                         digits = digits))
            chisqP <- as.numeric(format(binom$p.value, 
                                        digits = digits))
          }
          else {
            if (abs((route$estimate[1] - route$estimate[2])) < 
                1) {
              chisq <- prop.test(x = round(abs((route$estimate[1] - 
                                                  route$estimate[2])) * (length(var1) + 
                                                                           length(var2))), n = length(var1) + length(var2), 
                                 conf.level = cl, correct = FALSE)
            }
            else {
              chisq <- prop.test(x = round(abs((route$estimate[1] - 
                                                  route$estimate[2]))), n = length(var1) + 
                                   length(var2), conf.level = cl, correct = FALSE)
            }
            chisqStat <- as.numeric(format(chisq$statistic, 
                                           digits = digits))
            chisqdf <- as.numeric(format(chisq$parameter, 
                                         digits = digits))
            chisqP <- as.numeric(format(chisq$p.value, 
                                        digits = digits))
            propSter <- sd(var1, na.rm = TRUE) * sqrt(1/length(var1))
            propCIL <- route$estimate[1] - route$estimate[2] - 
              qnorm(cl) * propSter
            propCIH <- route$estimate[1] - route$estimate[2] + 
              qnorm(cl) * propSter
            propCIL <- as.numeric(format(propCIL, digits = digits))
            propCIH <- as.numeric(format(propCIH, digits = digits))
            propSter <- as.numeric(format(propSter, digits = digits))
          }
        }
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
        main3 <- matrix(c(paste(myargs[3], " = ", byt[a], 
                                sep = ""), ns[1], msg[1], mns[1], ster[1], 
                          stdev[1], paste("[", lci[1], ", ", hci[1], 
                                          "]", sep = ""), " ", paste(myargs[3], " = ", 
                                                                     byt[b], sep = ""), ns[2], msg[2], mns[2], 
                          ster[2], stdev[2], paste("[", lci[2], ", ", 
                                                   hci[2], "]", sep = ""), " ", "Difference", 
                          ns[3], msg[3], mns[3], ster[3], stdev[3], paste("[", 
                                                                          lci[3], ", ", hci[3], "]", sep = ""), paste("[", 
                                                                                                                      propCIL, ", ", propCIH, "]", sep = "")), 
                        ncol = 8, byrow = TRUE)
        main3 <- data.frame(main3)
        names(main3) <- c("Group", "Obs", "Missing", 
                          "Mean", "Std. Err.", "Std. Dev.", paste(conf.level * 
                                                                    100, "% CI", sep = ""), paste(conf.level * 
                                                                                                    100, "% CI", " [Wald]", sep = ""))
        row.names(main3) <- c("", " ", "  ")
        main4 <- matrix(c(paste(myargs[3], " = ", byt[a], 
                                sep = ""), ns[1], msg[1], mns[1], ster[1], 
                          stdev[1], paste("[", lci[1], ", ", hci[1], 
                                          "]", sep = ""), " ", paste(myargs[3], " = ", 
                                                                     byt[b], sep = ""), ns[2], msg[2], mns[2], 
                          ster[2], stdev[2], paste("[", lci[2], ", ", 
                                                   hci[2], "]", sep = ""), " ", "Difference", 
                          ns[3], msg[3], mns[3], ster[3], stdev[3], paste("[", 
                                                                          lci[3], ", ", hci[3], "]", sep = ""), paste("[", 
                                                                                                                      propCIL, ", ", propCIH, "]", sep = "")), 
                        ncol = 8, byrow = TRUE)
        main4 <- data.frame(main4)
        names(main4) <- c("Group", "Obs", "Missing", 
                          "Mean", "Std. Err.", "Std. Dev.", paste(conf.level * 
                                                                    100, "% CI", sep = ""), paste(conf.level * 
                                                                                                    100, "% CI", " [Exact Binomial]", sep = ""))
        row.names(main4) <- c("", " ", "  ")
        dfr <- as.numeric(format(dfr[3], digits = digits))
      }
    }
    par <- c(geom = geom, null.hypoth = null.hypoth, test.type = test.type, 
             var.eq = var.eq, conf.level = conf.level, matched = matched, 
             digits = digits)
    chisqTest <- c(prop = prop, exact = exact, chisqStat = chisqStat, 
                   chisqdf = chisqdf, chisqP = chisqP)
    invisible(list(tab = main, df = dfr, p = pval, tstat = tstat, 
                   var1 = var1, var2 = var2, by = by, par = par, geo = main2, 
                   wald = main3, exact = main4, chisqTest = chisqTest))
  }
  
  
  myargs <- c(deparse(substitute(var1)), deparse(substitute(var2)), 
              deparse(substitute(by)))
  out <- list()
  uby <- unique(by)
  if (is.null(strat)) {
    if (is.na(var2[1]) & length(var2) == 1) {
      if (is.na(by[1]) & length(by) == 1) {
        if (matched == TRUE) {
        }
        if (matched == FALSE) {
          strat <- rep(1, length(var1))
        }
      }
      if (length(by) > 1) {
        if (matched == TRUE) {
        }
        if (matched == FALSE) {
          strat <- rep(1, length(var1))
        }
      }
    }
    if (length(var2) > 1) {
      if (is.na(by[1]) & length(by) == 1) {
        if (matched == TRUE) {
          strat <- rep(1, length(var1))
        }
        if (matched == FALSE) {
          strat1 <- rep(1, length(var1))
          strat2 <- rep(1, length(var2))
        }
      }
      if (length(by) > 1) {
        if (matched == TRUE) {
        }
        if (matched == FALSE) {
        }
      }
    }
  }
  if (!is.null(strat)) {
    if (is.na(var2[1]) & length(var2) == 1) {
      if (length(by) > 1) {
        if (matched == FALSE) {
          strat1 <- unlist(strat[1])
          strat2 <- unlist(strat[2])
        }
      }
    }
    if (length(var2) > 1) {
      if (is.na(by[1]) & length(by) == 1) {
        if (matched == FALSE) {
          strat1 <- unlist(strat[1])
          strat2 <- unlist(strat[2])
        }
      }
    }
  }
  if (is.na(var2[1]) & length(var2) == 1) {
    if (is.na(by[1]) & length(by) == 1) {
      if (matched == TRUE) {
      }
      if (matched == FALSE) {
        ustrat <- unique(strat)
        for (t in 1:length(ustrat)) {
          x <- subset(var1, strat == ustrat[t])
          if (length(ustrat) > 1) {
            cat("\nStratum Value:")
            cat(ustrat[t])
          }
          ttest.obj <- ttest.do(var1 = x, var2 = var2, 
                                geom = geom, prop = prop, exact = exact, 
                                by = by, null.hypoth = null.hypoth, test.type = test.type, 
                                var.eq = var.eq, conf.level = conf.level, 
                                matched = matched, more.digits = more.digits, 
                                myargs = myargs)
          ttest.obj$call <- match.call()
          class(ttest.obj) <- "ttest"
          print(ttest.obj)
        }
      }
    }
    if (length(by) > 1) {
      if (matched == TRUE) {
      }
      if (matched == FALSE) {
        ustrat <- unique(strat)
        for (t in 1:length(ustrat)) {
          x <- subset(var1, strat == ustrat[t])
          cby <- subset(by, strat == ustrat[t])
          if (length(ustrat) > 1) {
            cat("\nStratum Value:")
            cat(ustrat[t])
          }
          ttest.obj <- ttest.do(var1 = x, var2 = var2, 
                                geom = geom, prop = prop, exact = exact, 
                                by = cby, null.hypoth = null.hypoth, test.type = test.type, 
                                var.eq = var.eq, conf.level = conf.level, 
                                matched = matched, more.digits = more.digits, 
                                myargs = myargs)
          ttest.obj$call <- match.call()
          class(ttest.obj) <- "ttest"
          print(ttest.obj)
        }
      }
    }
  }
  if (length(var2) > 1) {
    if (is.na(by[1]) & length(by) == 1) {
      if (matched == TRUE) {
        ustrat <- unique(strat)
        for (t in 1:length(ustrat)) {
          x <- subset(var1, strat == ustrat[t])
          y <- subset(var2, strat == ustrat[t])
          if (length(ustrat) > 1) {
            cat("\nStratum Value:")
            cat(ustrat[t])
          }
          ttest.obj <- ttest.do(x, y, geom = geom, prop = prop, 
                                exact = exact, by = by, null.hypoth, test.type, 
                                var.eq, conf.level, matched, more.digits, 
                                myargs)
          ttest.obj$call <- match.call()
          class(ttest.obj) <- "ttest"
          print(ttest.obj)
        }
      }
      if (matched == FALSE) {
        ustrat <- unique(c(strat1, strat2))
        for (t in 1:length(ustrat)) {
          x <- subset(var1, strat1 == ustrat[t])
          y <- subset(var2, strat2 == ustrat[t])
          if (length(ustrat) > 1) {
            cat("\nStratum Value:")
            cat(ustrat[t])
          }
          
          ttest.obj <- ttest.do(x, y, geom = geom, prop = prop, 
                                exact = exact, by = by, null.hypoth, test.type, 
                                var.eq, conf.level, matched, more.digits, 
                                myargs)
          ttest.obj$call <- match.call()
          class(ttest.obj) <- "ttest"
          print(ttest.obj)
        }
      }
    }
    if (length(by) > 1) {
      if (matched == TRUE) {
      }
      if (matched == FALSE) {
      }
    }
  }
}