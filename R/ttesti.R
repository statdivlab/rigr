#' T-test Given Summary Statistics with Improved Layout
#' 
#' Performs a one- or two-sample t-test given summary statistics. 
#' In the two-sample case, the user can specify
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
#' Defaults to 0.95.
#' @param var.eq a logical value, either
#' \code{TRUE} or \code{FALSE} (default), specifying whether or not equal
#' variances should be presumed in a two-sample t-test. 
#' @param more.digits a numeric value
#' specifying whether or not to display more or fewer digits in the output.
#' Non-integers are automatically rounded down. 
#' 
#' @return a list of class \code{ttesti}. The print method lays out the information in an easy-to-read
#' format. 
#' \item{tab}{A formatted table of descriptive and inferential statistics (number of observations,
#' mean, standard error of the mean estimate, standard deviation), 
#' along with a confidence interval for the mean.}
#' \item{df}{Degrees of freedom for the t-test.}
#' \item{p}{P-value for the t-test.}
#' \item{tstat}{Test statistic for the t-test.}
#' \item{par}{A vector of information about the type of test (null hypothesis, alternative hypothesis, etc.)}
#' \item{twosamp}{A logical value indicating whether a two-sample test was performed.}
#' \item{call}{The call made to the \code{ttesti} function.}
#' 
#' @examples
#' 
#' # t-test given sample descriptives
#' ttesti(24, 175, 35, null.hypoth=230)
#' 
#' # two-sample test
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
    if (length(obs) > 1 || length(obs2) > 1 || 
        length(mean) > 1 || length(mean2) > 1 ||
        length(sd) > 1 || length(sd2) > 1){
      stop("'obs', 'mean', and 'sd' must be variables of length 1.")
    } 
    if (!(obs%%1 == 0) || obs<= 0 || (!is.na(obs2) && (!(obs2%%1 == 0) || obs2 <= 0))){
      stop("Number of observations must be a positive integer.")
    }
    if (!is.scalar(mean) || (!is.na(mean2) && !is.scalar(mean2))){
      stop("Mean must be scalar.")
    }
    if (!is.scalar(sd) || (!is.na(sd2) && !is.scalar(sd2))){
      stop("SD must be scalar.")
    }
    if(is.na(obs2) && (!is.na(mean2) || !is.na(sd2))) {
      stop("A second number of observations must be entered for two sample test")
    }
    if(!is.na(obs2) && (is.na(mean2) || is.na(sd2))){
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
    if (!is.scalar(more.digits)) {
      stop("Argument 'more.digits' must be numeric")
    }

    if (!is.scalar(null.hypoth)){
    #if (!is.numeric(null.hypoth) ||  length(null.hypoth) > 1 || !is.finite(null.hypoth)){
      stop("Null must be a scalar.")
    }
    if (!(is.scalar(conf.level) && (conf.level > 0) && (conf.level < 1))){
      stop("'conf.level' must be a single number between 0 and 1")
    } 
    more.digits <- max(floor(more.digits), 0)
    digits <- 3 + more.digits
    cl <- conf.level+(1-conf.level)/2
    twosamp <- FALSE
    if(is.na(obs2)){
      stdErr <- sd/sqrt(obs)
      CIlower <- mean-stats::qt(conf.level+(1-conf.level)/2, obs-1)*stdErr
      CIupper <- mean+stats::qt(conf.level+(1-conf.level)/2, obs-1)*stdErr
      tstat <- (mean-null.hypoth)/(stdErr)
      printMat <- matrix(c(obs, 
                           format(mean, digits = digits), 
                           format(stdErr, digits = digits), 
                           format(sd, digits = digits), 
                           paste("[",format(CIlower, digits = digits),", ", 
                                 format(CIupper, digits=digits), "]", sep="")), nrow=1)
      colnames(printMat) <- c("Obs", "Mean", "Std. Error", "Std. Dev.", paste0(conf.level*100, "% CI"))
      rownames(printMat) <- "var1"
      
      df <- obs - 1
      if(alternative == "two.sided"){
        p <- 2*stats::pt(-abs(tstat), obs-1)
      } else if (alternative == "less"){
        p <- stats::pt(tstat, obs - 1)
      } else {
        p <- 1-stats::pt(tstat, obs-1)
      }
    } else{
      twosamp <- TRUE
      if(var.eq){
        stdErr1 <- sd/sqrt(obs)
        stdErr2 <- sd2/sqrt(obs2)
        CIlower1 <- mean-stats::qt(conf.level+(1-conf.level)/2, obs-1)*stdErr1
        CIupper1 <- mean+stats::qt(conf.level+(1-conf.level)/2, obs-1)*stdErr1
        CIlower2 <- mean2-stats::qt(conf.level+(1-conf.level)/2, obs2-1)*stdErr2
        CIupper2 <- mean2+stats::qt(conf.level+(1-conf.level)/2, obs2-1)*stdErr2
        stdErrDiff <- sqrt((sd^2*(obs-1)+sd2^2*(obs2-1))/(obs+obs2-2))*sqrt(1/obs+1/obs2)
        tstat <- (mean-mean2)/(stdErrDiff)
        CIlowerDiff <- (mean-mean2)-stats::qt(conf.level+(1-conf.level)/2, obs+obs2-2)*stdErrDiff
        CIupperDiff <- (mean-mean2)+stats::qt(conf.level+(1-conf.level)/2, obs+obs2-2)*stdErrDiff
        
        printMat <- matrix(c(obs, 
                             format(mean,digits = digits), 
                             format(stdErr1, digits = digits), 
                             format(sd, digits = digits), 
                             paste("[",format(CIlower1, digits = digits),", ", 
                                   format(CIupper1, digits=digits), "]", sep="")), nrow=1)
        colnames(printMat) <- c("Obs", "Mean", "Std. Error", "Std. Dev.", paste0(conf.level*100, "% CI"))
        printMat <- rbind(printMat, c(obs2, 
                                      format(mean2,digits = digits), 
                                      format(stdErr2, digits = digits), 
                                      format(sd2, digits = digits), 
                                      paste("[",format(CIlower2, digits = digits),", ", 
                                            format(CIupper2, digits = digits), "]", sep="")))
        printMat <- rbind(printMat, c(obs+obs2, 
                                      format(mean-mean2,digits = digits), 
                                      format(stdErrDiff, digits = digits), "<NA>", 
                                      paste("[",format(CIlowerDiff, digits = digits),", ", 
                                            format(CIupperDiff, digits = digits), "]", sep="")))
        rownames(printMat) <- c("var1", "var2", "diff")
        
        df <- obs+obs2-2

        if(alternative == "two.sided"){
          p <- 2*stats::pt(-abs(tstat), obs+obs2-2)
        } else if (alternative == "less"){
          p <- stats::pt(tstat, obs + obs2 - 2)
        } else {
          p <- 1-stats::pt(tstat, obs+obs2-2)
        }
      } else {
        stdErr1 <- sd/sqrt(obs)
        stdErr2 <- sd2/sqrt(obs2)
        CIlower1 <- mean-stats::qt(conf.level+(1-conf.level)/2, obs-1)*stdErr1
        CIupper1 <- mean+stats::qt(conf.level+(1-conf.level)/2, obs-1)*stdErr1
        CIlower2 <- mean2-stats::qt(conf.level+(1-conf.level)/2, obs2-1)*stdErr2
        CIupper2 <- mean2+stats::qt(conf.level+(1-conf.level)/2, obs2-1)*stdErr2
        stdErrDiff <- sqrt(sd^2/(obs)+sd2^2/(obs2))
        tstat <- (mean-mean2)/(stdErrDiff)
        df <- (sd^2/obs+sd2^2/obs2)^2/((sd^2/obs)^2/(obs-1)+(sd2^2/obs2)^2/(obs2-1))
        CIlowerDiff <- (mean-mean2)-stats::qt(conf.level+(1-conf.level)/2, obs+obs2-2)*stdErrDiff
        CIupperDiff <- (mean-mean2)+stats::qt(conf.level+(1-conf.level)/2, obs+obs2-2)*stdErrDiff
        
        printMat <- matrix(c(obs, 
                             format(mean, digits =digits), 
                             format(stdErr1, digits =digits), 
                             format(sd, digits =digits), 
                             paste("[",format(CIlower1, digits =digits),", ", 
                                   format(CIupper1, digits =digits), "]", sep="")), nrow=1)
        colnames(printMat) <- c("Obs", "Mean", "Std. Error", "Std. Dev.", paste0(conf.level*100, "% CI"))
        printMat <- rbind(printMat,c(obs2, 
                                     format(mean2, digits =digits), 
                                     format(stdErr2, digits =digits), 
                                     format(sd2, digits =digits), 
                                     paste("[",format(CIlower2, digits = digits),", ", 
                                           format(CIupper2, digits =digits), "]", sep="")))
        printMat <- rbind(printMat,
                          c(obs+obs2, format(mean-mean2, digits =digits), 
                            format(stdErrDiff, digits =digits), "<NA>", 
                            paste("[",format(CIlowerDiff, digits =digits),", ", 
                                  format(CIupperDiff, digits =digits), "]", sep="")))
        rownames(printMat) <- c("var1", "var2", "diff")

        if(alternative == "two.sided"){
          p <- 2*stats::pt(-abs(tstat), df)
        } else if (alternative == "less"){
          p <- stats::pt(tstat, df)
        } else {
          p <- 1-stats::pt(tstat, df)
        }
      }
    }
    tstat <- as.numeric(format(tstat, digits = max(digits + 
                                                     1, 3)))
    p <- as.numeric(format(p, digits = max(digits, 
                                            digits + 3)))
    df <- as.numeric(format(df, digits = max(digits, 
                                            digits + 3)))
    par <- c(null.hypoth = null.hypoth, alternative = alternative, 
             var.eq = var.eq, conf.level = conf.level, twosamp = twosamp,
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
