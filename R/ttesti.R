#' T-test Given Descriptive Statistics with Improved Layout
#' 
#' Produces table of relevant descriptive statistics and inference for either
#' one- or two-sample t-test. In the two-sample case, the user can specify
#' whether or not equal variances should be presumed. 
#' 
#' If \code{obs2}, \code{mean2}, or \code{sd2} is specified, then all three must be specified
#' and a two-sample t-test is run.
#' 
#' @aliases ttesti ttesti.do print.ttesti ttesti.default
#' 
#' @param obs number of observations for the
#' first sample.
#' @param mean the sample mean of the first
#' sample.
#' @param sd the sample standard deviation of
#' the first sample.
#' @param obs2 number of observations for the second sample (this is optional).
#' @param mean2 if \code{obs2} is supplied, then sample mean of the second
#' sample must be supplied.
#' @param sd2 if \code{obs2} is supplied, then sample standard deviation of the
#' second sample must be supplied.
#' @param null.hypoth  a number specifying the
#' null hypothesis for the mean (or difference in means if performing a
#' two-sample test). Defaults to zero.
#' @param alternative a string: one of
#' \code{"less"}, \code{"two.sided"}, or \code{"greater"} specifying the form
#' of the test. Defaults to a two-sided test.
#' @param conf.level confidence level of the test.
#' Defaults to 95/100.
#' @param var.eq a logical value, either
#' \code{TRUE} or \code{FALSE} (default), specifying whether or not equal
#' variances should be presumed in a two-sample t-test. 
#' @param more.digits a numeric value
#' specifying whether or not to display more or fewer digits in the output.
#' Non-integers are automatically rounded down. 
#' 
#' @return Prints a summary of the data and the
#' corresponding t-test. \item{Variable}{\code{x} in a
#' one-sample test, or \code{x} and \code{y} in a two sample test. The first
#' set of descriptives entered goes to \code{x}.} \item{Obs}{Number of
#' observations of each variable: includes missing values.} \item{Mean}{the
#' sample mean; also, the estimated difference in means in a two-sample test.}
#' \item{Std.Err.}{the estimated standard error of the mean and of the
#' difference in the two-sample test.} \item{Std.Dev.}{standard deviation
#' estimates.} \item{CI}{a confidence interval for the means, and for the
#' difference in the two-sample test. This is at the confidence level specified
#' in the argument.}
#' \item{Null hypothesis}{a statement of the null hypothesis.}
#' \item{Alternative hypothesis}{a statement of the alternative hypothesis.}
#' \item{t}{value of the t-statistic.} \item{df}{the degrees of freedom for the
#' test.} \item{Pr}{a p-value for inference on the corresponding hypothesis
#' test.}
#' 
#' @examples
#' 
#' #- T-test given sample descriptives -#
#' ttesti(24, 175, 35, null.hyp=230)
#' 
#' #- Two-sample test -#
#' ttesti(10, -1.6, 1.5, 30, -.7, 2.1)
#' 
#' @export ttesti
ttesti <- function(obs, 
                   mean, 
                   sd, 
                   obs2=NA, 
                   mean2=NA, 
                   sd2=NA, 
                   null.hypoth = 0, 
                   conf.level=.95, 
                   alternative="two.sided", 
                   var.eq = FALSE,
                   more.digits = 0){
  ttesti.do <- function(obs, mean, sd,
                        obs2, mean2, sd2,
                        null.hypoth, conf.level,
                        alternative, var.eq, more.digits, ...) {
    if(is.na(obs2) & (!is.na(mean2) | !is.na(sd2))) {
      stop("A second number of observations must be entered for two sample test")
    }
    if(!is.na(obs2) & (is.na(mean2) | is.na(sd2))){
      stop("SD and mean for the second sample must be entered for two sample test")
    }
    # check that alternative is one of "less", "two.sided", or "greater"
    if (!(alternative %in% c("less", "two.sided", "greater"))) {
      stop("'alternative' must be either 'less', 'two.sided', or 'greater'")
    }
    # make sure var.eq is either true or false
    if (!is.logical(var.eq)) {
      stop("Please specify a logical value for variable 'var.eq'")
    }
    # check to make sure additional digit request is numeric
    if (!is.numeric(more.digits)) {
      stop("Argument 'more.digits' must be numeric")
    }
    if (!(obs%%1 == 0) || obs<= 0 || (!is.na(obs2) && (!(obs2%%1 == 0) || obs2 <= 0))){
      stop("Number of observations must be a positive integer.")
    }
    if (!is.numeric(mean) || (!is.na(mean2) && !is.numeric(mean2))){
      stop("Mean must be numeric.")
    }
    if (!is.numeric(sd) || (!is.na(sd2) && !is.numeric(sd2))){
      stop("SD must be numeric.")
    }
    if (!is.numeric(null.hypoth) ||  length(null.hypoth) > 1 || !is.finite(null.hypoth)){
      stop("Null must be a scalar.")
    }
    if (!((length(conf.level) == 1L) && is.finite(conf.level) && 
          (conf.level > 0) && (conf.level < 1))){
      stop("'conf.level' must be a single number between 0 and 1")
    } 
    more.digits <- max(floor(more.digits), 0)
    digits <- 3 + more.digits
    cl <- conf.level+(1-conf.level)/2
    if(is.na(obs2)){
      stdErr <- sd/sqrt(obs)
      CIlower <- mean-qt(conf.level+(1-conf.level)/2, obs-1)*stdErr
      CIupper <- mean+qt(conf.level+(1-conf.level)/2, obs-1)*stdErr
      tstat <- (mean-null.hypoth)/(stdErr)
      printMat <- matrix(c(obs, 
                           round(mean,2), 
                           round(stdErr, 2), 
                           sd, 
                           paste("[",round(CIlower, 2),", ", round(CIupper, 2), "]", sep="")), nrow=1)
      colnames(printMat) <- c("Obs", "Mean", "Std. Error", "SD", paste(conf.level*100, "%CI"))
      rownames(printMat) <- "x"
      
      df <- obs - 1
      if(alternative == "two.sided"){
        p <- 2*pt(-abs(tstat), obs-1)
      } else if (alternative == "less"){
        p <- pt(-abs(tstat), obs - 1)
      } else {
        p <- 1-pt(abs(tstat), obs-1)
      }
    } else{
      if(var.eq){
        stdErr1 <- sd/sqrt(obs)
        stdErr2 <- sd2/sqrt(obs2)
        CIlower1 <- mean-qt(conf.level+(1-conf.level)/2, obs-1)*stdErr1
        CIupper1 <- mean+qt(conf.level+(1-conf.level)/2, obs-1)*stdErr1
        CIlower2 <- mean2-qt(conf.level+(1-conf.level)/2, obs2-1)*stdErr2
        CIupper2 <- mean2+qt(conf.level+(1-conf.level)/2, obs2-1)*stdErr2
        stdErrDiff <- sqrt((sd^2*(obs-1)+sd2^2*(obs2-1))/(obs+obs2-2))*sqrt(1/obs+1/obs2)
        tstat <- (mean-mean2)/(stdErrDiff)
        CIlowerDiff <- (mean-mean2)-qt(conf.level+(1-conf.level)/2, obs+obs2-2)*stdErrDiff
        CIupperDiff <- (mean-mean2)+qt(conf.level+(1-conf.level)/2, obs+obs2-2)*stdErrDiff
        
        printMat <- matrix(c(obs, 
                             round(mean,2), 
                             round(stdErr1, 2), 
                             sd, 
                             paste("[",round(CIlower1, 2),", ", round(CIupper1, 2), "]", sep="")), nrow=1)
        colnames(printMat) <- c("Obs", "Mean", "Std. Error", "SD", paste(conf.level*100, "%CI"))
        printMat <- rbind(printMat, c(obs2, 
                                      round(mean2,2), 
                                      round(stdErr2, 2), 
                                      sd2, 
                                      paste("[",round(CIlower2, 2),", ", round(CIupper2, 2), "]", sep="")))
        printMat <- rbind(printMat, c(obs+obs2, 
                                      round(mean-mean2,2), 
                                      round(stdErrDiff, 2), "<NA>", 
                                      paste("[",round(CIlowerDiff, 2),", ", 
                                            round(CIupperDiff, 2), "]", sep="")))
        rownames(printMat) <- c("x", "y", "diff")
        
        df <- obs+obs2-2

        if(alternative == "two.sided"){
          p <- 2*pt(-abs(tstat), obs+obs2-2)
        } else if (alternative == "less"){
          p <- pt(tstat, obs + obs2 - 2)
        } else {
          p <- 1-pt(abs(tstat), obs+obs2-2)
        }
      } else {
        stdErr1 <- sd/sqrt(obs)
        stdErr2 <- sd2/sqrt(obs2)
        CIlower1 <- mean-qt(conf.level+(1-conf.level)/2, obs-1)*stdErr1
        CIupper1 <- mean+qt(conf.level+(1-conf.level)/2, obs-1)*stdErr1
        CIlower2 <- mean2-qt(conf.level+(1-conf.level)/2, obs2-1)*stdErr2
        CIupper2 <- mean2+qt(conf.level+(1-conf.level)/2, obs2-1)*stdErr2
        stdErrDiff <- sqrt(sd^2/(obs)+sd2^2/(obs2))
        tstat <- (mean-mean2)/(stdErrDiff)
        df <- (sd^2/obs+sd2^2/obs2)^2/((sd^2/obs)^2/(obs-1)+(sd2^2/obs2)^2/(obs2-1))
        CIlowerDiff <- (mean-mean2)-qt(conf.level+(1-conf.level)/2, obs+obs2-2)*stdErrDiff
        CIupperDiff <- (mean-mean2)+qt(conf.level+(1-conf.level)/2, obs+obs2-2)*stdErrDiff
        
        printMat <- matrix(c(obs, 
                             round(mean, 2), 
                             round(stdErr1, 2), 
                             sd, 
                             paste("[",round(CIlower1, 2),", ", round(CIupper1, 2), "]", sep="")), nrow=1)
        colnames(printMat) <- c("Obs", "Mean", "Std. Error", "SD", paste(conf.level*100, "%CI"))
        printMat <- rbind(printMat,c(obs2, 
                                     round(mean2, 2), 
                                     round(stdErr2, 2), 
                                     sd2, 
                                     paste("[",round(CIlower2, 2),", ", round(CIupper2, 2), "]", sep="")))
        printMat <- rbind(printMat,
                          c(obs+obs2, round(mean-mean2, 2), round(stdErrDiff, 2), "<NA>", 
                            paste("[",round(CIlowerDiff, 2),", ", round(CIupperDiff, 2), "]", sep="")))
        rownames(printMat) <- c("x", "y", "diff")

        if(alternative == "two.sided"){
          p <- 2*pt(-abs(tstat), df)
        } else if (alternative == "less"){
          p <- pt(tstat, df)
        } else {
          p <- 1-pt(abs(tstat), df)
        }
      }
    }
    par <- c(null.hypoth = null.hypoth, alternative = alternative, 
             var.eq = var.eq, conf.level = conf.level, 
             digits = digits)
    invisible(list(tab = printMat, df = df, p = p, tstat = tstat, 
                   par = par))
  }
  myargs <- c(deparse(substitute(obs)), deparse(substitute(obs2)))
  ttesti.obj <- ttesti.do(obs = obs, mean = mean, sd = sd,
                          obs2 = obs2, mean2 = mean2, sd2 = sd2,
                          null.hypoth = null.hypoth, conf.level = conf.level,
                          alternative = alternative, var.eq = var.eq, more.digits = more.digits)
  ttesti.obj$call <- match.call()
  class(ttesti.obj) <- "ttesti"
  return(ttesti.obj)
} 
