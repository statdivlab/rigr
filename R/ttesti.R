## T-test when given summary statistics
## Args:          obs - the number observed
##               mean - the sample mean
##                 sd - sample standard deviation
##           null.hyp - null hypothesis
##        alternative - alternative hypothesis
## Returns: Table with the t-test, t-stat, degrees of freedom
ttesti <- function(obs, mean, sd, obs2=NA, mean2=NA, sd2=NA, null.hyp = 0, level=.95, alternative="two.sided", var.eq = FALSE, prop=FALSE, exact=FALSE){
  if(is.na(obs)|is.na(mean)|is.na(sd)) stop("A value for obs, mean, and sd must be entered")
  if(is.na(obs2) & (!is.na(mean2) | !is.na(sd2))){stop("A second observation must be entered for two sample test")}
  if(!is.na(obs2) & (is.na(mean2) | is.na(sd2))){stop("SD and mean must be entered for two sample test")}
  if(exact & !prop){stop("Exact binomial confidence intervals cannot be computed for anything but a proportion.")}
  digits <- 3
  cl <- level+(1-level)/2
  if(is.na(obs2)){
    stdErr <- sd/sqrt(obs)
    CIlower <- mean-qt(level+(1-level)/2, obs-1)*stdErr
    CIupper <- mean+qt(level+(1-level)/2, obs-1)*stdErr
    tStat <- (mean-null.hyp)/(stdErr)
    propSter <- NULL
    propCIL <- NULL
    propCIH <- NULL
    chisqStat <- NULL
    chisqP <- NULL
    chisqdf <- NULL  
    if(prop){
      if(exact){
        if(abs(mean) < 1){
          binom <- binom.test(x=round(abs(mean*obs)), n=obs, p=null.hyp+.0000000001,alternative=alternative, conf.level=cl)
          propCIL <- as.numeric(format(min(binom$conf.int), digits=digits))
          propCIH <- as.numeric(format(max(binom$conf.int), digits=digits))
        } else {
          binom <- binom.test(x=round(abs(mean)), n=obs, p=null.hyp+.0000000001,alternative=alternative, conf.level=cl)
          propCIL <- as.numeric(format(min(binom$conf.int)*obs, digits=digits))
          propCIH <- as.numeric(format(max(binom$conf.int)*obs, digits=digits))
        }
        chisqStat <- as.numeric(format(binom$statistic, digits=digits))
        chisqdf <- as.numeric(format(binom$parameter, digits=digits))
        chisqP <- as.numeric(format(binom$p.value, digits=digits))
        
      } else {
        if(abs(mean) < 1){
          chisq <- prop.test(x=round(abs(mean*obs)), n=obs, conf.level=cl, correct=FALSE)
        } else{
          chisq <- prop.test(x=round(abs(mean)), n=obs, conf.level=cl, correct=FALSE)
        }
        chisqStat <- as.numeric(format(chisq$statistic, digits=digits))
        chisqdf <- as.numeric(format(chisq$parameter, digits=digits))
        chisqP <- as.numeric(format(chisq$p.value, digits=digits))
        propSter <- sd*sqrt(1/obs)
        propCIL <- mean - qnorm(cl)*propSter
        propCIH <- mean + qnorm(cl)*propSter
        propCIL <- as.numeric(format(propCIL, digits=digits))
        propCIH <- as.numeric(format(propCIH, digits=digits))
        propSter <- as.numeric(format(propSter, digits=digits))
      }
    }
    cat("\n", "One-sample t-test", "\n")
    printMat <- matrix(c(obs, round(mean,2), round(stdErr, 2), sd, paste("[",round(CIlower, 2),", ", round(CIupper, 2), "]", sep="")), nrow=1)
    colnames(printMat) <- c("Obs", "Mean", "Std. Error", "SD", paste(level*100, "%CI"))
    rownames(printMat) <- "x"
    printMat2 <- matrix(c(obs, round(mean,2), round(stdErr, 2), sd, paste("[",round(CIlower, 2),", ", round(CIupper, 2), "]", sep=""), paste("[",propCIL,", ", propCIH, "]", sep="")), nrow=1)
    colnames(printMat2) <- c("Obs", "Mean", "Std. Error", "SD", paste(level*100, "%CI"), paste(level*100, "% CI", "[Wald]", sep=""))
    rownames(printMat2) <- "x"
    printMat3 <- matrix(c(obs, round(mean,2), round(stdErr, 2), sd, paste("[",round(CIlower, 2),", ", round(CIupper, 2), "]", sep=""), paste("[",propCIL,", ", propCIH, "]", sep="")), nrow=1)
    colnames(printMat3) <- c("Obs", "Mean", "Std. Error", "SD", paste(level*100, "%CI"), paste(level*100, "% CI", "[Exact Binomial]", sep=""))
    rownames(printMat3) <- "x"
    if(prop){
      if(exact){
        print(printMat3, quote=FALSE)
      } else{
        print(printMat2, quote=FALSE)
      }
    } else{
      print(printMat, quote=FALSE)
    }
    cat("\n", "t-statistic =", round(tStat, 3), ", df =", floor(obs-1), "\n")
    cat("\n", paste("Ho: mean = ", null.hyp),"\n")
    if(alternative == "two.sided"){
      cat("\n", paste("Ha: mean !=", null.hyp, ","), paste("Pr(|T| > |t|) =", format.pval(2*pt(-abs(tStat), obs-1), 5)), "\n")
    } else if (alternative == "less"){
      cat("\n", paste("Ha: mean <", null.hyp, ","), paste("Pr(T < t) =", format.pval(pt(-abs(tStat), obs-1), 5)), "\n")
    } else {
      cat("\n", paste("Ha: mean >", null.hyp, ","), paste("Pr(T > t) =", format.pval(1-pt(abs(tStat), obs-1), 5)), "\n")
    }
    if(prop){
      if(exact){
        cat("\n", "Test of Difference in Proportions:", "\n")
        cat("\n", paste("# of successes = ", chisqStat, ", ", "# of trials = ", chisqdf, sep=""), "\n")
        cat("\n", paste("p-value = ", chisqP, sep=""), "\n")
      } else {
        cat("\n", "Test of Difference in Proportions:", "\n")
        cat("\n", paste("Chi-squared = ", chisqStat, ", ", "df = ", chisqdf, sep=""), "\n")
        cat("\n", paste("p-value = ", chisqP, sep=""), "\n")
      }
    }
  } else{
    if(var.eq){
      stdErr1 <- sd/sqrt(obs)
      stdErr2 <- sd2/sqrt(obs2)
      CIlower1 <- mean-qt(level+(1-level)/2, obs-1)*stdErr1
      CIupper1 <- mean+qt(level+(1-level)/2, obs-1)*stdErr1
      CIlower2 <- mean2-qt(level+(1-level)/2, obs2-1)*stdErr2
      CIupper2 <- mean2+qt(level+(1-level)/2, obs2-1)*stdErr2
      stdErrDiff <- sqrt((sd^2*(obs-1)+sd2^2*(obs2-1))/(obs+obs2-2))*sqrt(1/obs+1/obs2)
      tStat <- (mean-mean2)/(stdErrDiff)
      CIlowerDiff <- (mean-mean2)-qt(level+(1-level)/2, obs+obs2-2)*stdErrDiff
      CIupperDiff <- (mean-mean2)+qt(level+(1-level)/2, obs+obs2-2)*stdErrDiff
      propSter <- NULL
      propCIL <- NULL
      propCIH <- NULL
      chisqStat <- NULL
      chisqP <- NULL
      chisqdf <- NULL  
      if(prop){
        if(exact){
          if(abs(mean-mean2) < 1){
            binom <- binom.test(x=round(abs((mean-mean2)*(obs+obs2))), n=obs+obs2, p=null.hyp+.0000000001,alternative=alternative, conf.level=cl)
            propCIL <- as.numeric(format(min(binom$conf.int), digits=digits))
            propCIH <- as.numeric(format(max(binom$conf.int), digits=digits))
          } else {
            binom <- binom.test(x=round(abs((mean-mean2))), n=obs+obs2, p=null.hyp+.0000000001,alternative=alternative, conf.level=cl)
            propCIL <- as.numeric(format(min(binom$conf.int)*(obs+obs2), digits=digits))
            propCIH <- as.numeric(format(max(binom$conf.int)*(obs+obs2), digits=digits))
          }
          chisqStat <- as.numeric(format(binom$statistic, digits=digits))
          chisqdf <- as.numeric(format(binom$parameter, digits=digits))
          chisqP <- as.numeric(format(binom$p.value, digits=digits))
          
        } else {
          if(abs(mean-mean2) < 1){
            chisq <- prop.test(x=round(abs((mean-mean2)*(obs+obs2))), n=obs+obs2, conf.level=cl, correct=FALSE)
          } else{
            chisq <- prop.test(x=round(abs(mean-mean2)), n=obs+obs2, conf.level=cl, correct=FALSE)
          }
          chisqStat <- as.numeric(format(chisq$statistic, digits=digits))
          chisqdf <- as.numeric(format(chisq$parameter, digits=digits))
          chisqP <- as.numeric(format(chisq$p.value, digits=digits))
          propSter <- stdErrDiff
          propCIL <- mean-mean2 - qnorm(cl)*propSter
          propCIH <- mean-mean2 + qnorm(cl)*propSter
          propCIL <- as.numeric(format(propCIL, digits=digits))
          propCIH <- as.numeric(format(propCIH, digits=digits))
          propSter <- as.numeric(format(propSter, digits=digits))
        }
      }
      cat("\n", "Two-sample t-test with equal variances", "\n")
      printMat <- matrix(c(obs, round(mean,2), round(stdErr1, 2), sd, paste("[",round(CIlower1, 2),", ", round(CIupper1, 2), "]", sep="")), nrow=1)
      colnames(printMat) <- c("Obs", "Mean", "Std. Error", "SD", paste(level*100, "%CI"))
      printMat <- rbind(printMat, c(obs2, round(mean2,2), round(stdErr2, 2), sd2, paste("[",round(CIlower2, 2),", ", round(CIupper2, 2), "]", sep="")))
      printMat <- rbind(printMat, c(obs+obs2, round(mean-mean2,2), round(stdErrDiff, 2), "<NA>", paste("[",round(CIlowerDiff, 2),", ", round(CIupperDiff, 2), "]", sep="")))
      rownames(printMat) <- c("x", "y", "diff")
      printMat2 <- cbind(printMat, c(" ", " " ,paste("[",propCIL,", ", propCIH, "]", sep="")))
      colnames(printMat2) <- c("Obs", "Mean", "Std. Error", "SD", paste(level*100, "%CI"), paste(level*100, "% CI", "[Wald]", sep=""))
      rownames(printMat2) <- c("x", "y", "diff")
      printMat3 <- cbind(printMat, c(" ", " " , paste("[",propCIL,", ", propCIH, "]", sep="")))
      colnames(printMat3) <- c("Obs", "Mean", "Std. Error", "SD", paste(level*100, "%CI"), paste(level*100, "% CI", "[Exact Binomial]", sep=""))
      rownames(printMat3) <- c("x", "y", "diff")
      if(prop){
        if(exact){
          print(printMat3, quote=FALSE)
        } else{
          print(printMat2, quote=FALSE)
        }
      } else{
        print(printMat, quote=FALSE)
      }
      cat("\n", "t-statistic =", round(tStat, 3), ", df =", floor(obs+obs2-2), "\n")
      cat("\n", paste("Ho: mean(x) - mean(y) = diff = ", null.hyp),"\n")
      if(alternative == "two.sided"){
        cat("\n", paste("Ha: diff !=", null.hyp, ","), paste("Pr(|T| > |t|) =", format.pval(2*pt(-abs(tStat), obs+obs2-2), 5)), "\n")
      } else if (alternative == "less"){
        cat("\n", paste("Ha: diff <", null.hyp, ","), paste("Pr(T < t) =", format.pval(pt(-abs(tStat), obs+obs2-2), 5)), "\n")
      } else {
        cat("\n", paste("Ha: diff >", null.hyp, ","), paste("Pr(T > t) =", format.pval(1-pt(abs(tStat), obs+obs2-2), 5)), "\n")
      }
      if(prop){
        if(exact){
          cat("\n", "Test of Difference in Proportions:", "\n")
          cat("\n", paste("# of successes = ", chisqStat, ", ", "# of trials = ", chisqdf, sep=""), "\n")
          cat("\n", paste("p-value = ", chisqP, sep=""), "\n")
        } else {
          cat("\n", "Test of Difference in Proportions:", "\n")
          cat("\n", paste("Chi-squared = ", chisqStat, ", ", "df = ", chisqdf, sep=""), "\n")
          cat("\n", paste("p-value = ", chisqP, sep=""), "\n")
        }
      }
    } else {
      stdErr1 <- sd/sqrt(obs)
      stdErr2 <- sd2/sqrt(obs2)
      CIlower1 <- mean-qt(level+(1-level)/2, obs-1)*stdErr1
      CIupper1 <- mean+qt(level+(1-level)/2, obs-1)*stdErr1
      CIlower2 <- mean2-qt(level+(1-level)/2, obs2-1)*stdErr2
      CIupper2 <- mean2+qt(level+(1-level)/2, obs2-1)*stdErr2
      stdErrDiff <- sqrt(sd^2/(obs)+sd2^2/(obs2))
      tStat <- (mean-mean2)/(stdErrDiff)
      df <- (sd^2/obs+sd2^2/obs2)^2/((sd^2/obs)^2/(obs-1)+(sd2^2/obs2)^2/(obs2-1))
      CIlowerDiff <- (mean-mean2)-qt(level+(1-level)/2, obs+obs2-2)*stdErrDiff
      CIupperDiff <- (mean-mean2)+qt(level+(1-level)/2, obs+obs2-2)*stdErrDiff
      propSter <- NULL
      propCIL <- NULL
      propCIH <- NULL
      chisqStat <- NULL
      chisqP <- NULL
      chisqdf <- NULL  
      if(prop){
        if(exact){
          if(abs(mean-mean2) < 1){
            binom <- binom.test(x=round(abs((mean-mean2)*(obs+obs2))), n=obs+obs2, p=null.hyp+.0000000001,alternative=alternative, conf.level=cl)
            propCIL <- as.numeric(format(min(binom$conf.int), digits=digits))
            propCIH <- as.numeric(format(max(binom$conf.int), digits=digits))
          } else {
            binom <- binom.test(x=round(abs((mean-mean2))), n=obs+obs2, p=null.hyp+.0000000001,alternative=alternative, conf.level=cl)
            propCIL <- as.numeric(format(min(binom$conf.int)*(obs+obs2), digits=digits))
            propCIH <- as.numeric(format(max(binom$conf.int)*(obs+obs2), digits=digits))
          }
          chisqStat <- as.numeric(format(binom$statistic, digits=digits))
          chisqdf <- as.numeric(format(binom$parameter, digits=digits))
          chisqP <- as.numeric(format(binom$p.value, digits=digits))
          
        } else {
          if(abs(mean-mean2) < 1){
            chisq <- prop.test(x=round(abs((mean-mean2)*(obs+obs2))), n=obs+obs2, conf.level=cl, correct=FALSE)
          } else{
            chisq <- prop.test(x=round(abs(mean-mean2)), n=obs+obs2, conf.level=cl, correct=FALSE)
          }
          chisqStat <- as.numeric(format(chisq$statistic, digits=digits))
          chisqdf <- as.numeric(format(chisq$parameter, digits=digits))
          chisqP <- as.numeric(format(chisq$p.value, digits=digits))
          propSter <- stdErrDiff
          propCIL <- mean-mean2 - qnorm(cl)*propSter
          propCIH <- mean-mean2 + qnorm(cl)*propSter
          propCIL <- as.numeric(format(propCIL, digits=digits))
          propCIH <- as.numeric(format(propCIH, digits=digits))
          propSter <- as.numeric(format(propSter, digits=digits))
        }
      }
      cat("\n", "Two-sample t-test with unequal variances", "\n")
      printMat <- matrix(c(obs, round(mean, 2), round(stdErr1, 2), sd, paste("[",round(CIlower1, 2),", ", round(CIupper1, 2), "]", sep="")), nrow=1)
      colnames(printMat) <- c("Obs", "Mean", "Std. Error", "SD", paste(level*100, "%CI"))
      printMat <- rbind(printMat,c(obs2, round(mean2, 2), round(stdErr2, 2), sd2, paste("[",round(CIlower2, 2),", ", round(CIupper2, 2), "]", sep="")))
      printMat <- rbind(printMat,c(obs+obs2, round(mean-mean2, 2), round(stdErrDiff, 2), "<NA>", paste("[",round(CIlowerDiff, 2),", ", round(CIupperDiff, 2), "]", sep="")))
      rownames(printMat) <- c("x", "y", "diff")
      printMat2 <- cbind(printMat, c(" ", " " ,paste("[",propCIL,", ", propCIH, "]", sep="")))
      colnames(printMat2) <- c("Obs", "Mean", "Std. Error", "SD", paste(level*100, "%CI"), paste(level*100, "% CI", "[Wald]", sep=""))
      rownames(printMat2) <- c("x", "y", "diff")
      printMat3 <- cbind(printMat, c(" ", " " , paste("[",propCIL,", ", propCIH, "]", sep="")))
      colnames(printMat3) <- c("Obs", "Mean", "Std. Error", "SD", paste(level*100, "%CI"), paste(level*100, "% CI", "[Exact Binomial]", sep=""))
      rownames(printMat3) <- c("x", "y", "diff")
      if(prop){
        if(exact){
          print(printMat3, quote=FALSE)
        } else{
          print(printMat2, quote=FALSE)
        }
      } else{
        print(printMat, quote=FALSE)
      }        
      cat("\n", "t-statistic =", round(tStat, 3), ", Satterthwaite's df =", floor(df), "\n")
      cat("\n", paste("Ho: mean(x) - mean(y) = diff = ", null.hyp),"\n")
      if(alternative == "two.sided"){
        cat("\n", paste("Ha: diff !=", null.hyp, ","), paste("Pr(|T| > |t|) =", format.pval(2*pt(-abs(tStat), df), 5)), "\n")
      } else if (alternative == "less"){
        cat("\n", paste("Ha: diff <", null.hyp, ","), paste("Pr(T < t) =", format.pval(pt(-abs(tStat), df), 5)), "\n")
      } else {
        cat("\n", paste("Ha: diff >", null.hyp, ","), paste("Pr(T > t) =", format.pval(1-pt(abs(tStat), df), 5)), "\n")
      }
      if(prop){
        if(exact){
          cat("\n", "Test of Difference in Proportions:", "\n")
          cat("\n", paste("# of successes = ", chisqStat, ", ", "# of trials = ", chisqdf, sep=""), "\n")
          cat("\n", paste("p-value = ", chisqP, sep=""), "\n")
        } else {
          cat("\n", "Test of Difference in Proportions:", "\n")
          cat("\n", paste("Chi-squared = ", chisqStat, ", ", "df = ", chisqdf, sep=""), "\n")
          cat("\n", paste("p-value = ", chisqP, sep=""), "\n")
        }
      }
    }
  }
} 