#' T-test Given Descriptive Statistics with Improved Layout
#' 
#' Produces table of relevant descriptive statistics and inference for either
#' one- or two-sample t-test. In the two-sample case, the user can specify
#' whether or not equal variances should be presumed. 
#' 
#' If \code{obs2}, \code{mean2}, or \code{sd2} is specified, then all three must be specified
#' and a two-sample t-test is run.
#' 
#' @aliases proptesti proptesti.do print.proptesti proptesti.default
#' 
#' @param x1 Number of successes in first sample
#' @param n1 Number of trials in first sample
#' @param x2 Number of successes in second sample
#' @param n2 Number of trials in second sample
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
#' #- Two-sample test -#
#' proptesti(10, 100, 15, 200, alternative = "less")
#' 
#' @export proptesti
proptesti <- function(x1, n1, x2 = NA, n2 = NA, exact = FALSE,
                   null.hypoth = 0.5, 
                   conf.level=.95, 
                   alternative="two.sided", 
                   more.digits = 0){
  proptesti.do <- function(x1, n1, x2 = NA, n2 = NA, exact = FALSE,
                           null.hypoth = 0.5, 
                           conf.level=.95, 
                           alternative="two.sided", 
                           more.digits = 0, ...) {
    #if (length(obs) > 1 || length(obs2) > 1 || 
    #    length(mean) > 1 || length(mean2) > 1 ||
    #    length(sd) > 1 || length(sd2) > 1){
    #  stop("'obs', 'mean', and 'sd' must be variables of length 1.")
    #} 
    # if (!(obs%%1 == 0) || obs<= 0 || (!is.na(obs2) && (!(obs2%%1 == 0) || obs2 <= 0))){
    #   stop("Number of observations must be a positive integer.")
    # }
    # if (!is.scalar(mean) || (!is.na(mean2) && !is.scalar(mean2))){
    #   stop("Mean must be scalar.")
    # }
    # if (!is.scalar(sd) || (!is.na(sd2) && !is.scalar(sd2))){
    #   stop("SD must be scalar.")
    # }
    # if(is.na(obs2) && (!is.na(mean2) || !is.na(sd2))) {
    #   stop("A second number of observations must be entered for two sample test")
    # }
    # if(!is.na(obs2) && (is.na(mean2) || is.na(sd2))){
    #   stop("SD and mean for the second sample must be entered for two sample test")
    # }
    # # check that alternative is one of "less", "two.sided", or "greater"
    # if (!(alternative %in% c("less", "two.sided", "greater"))) {
    #   stop("'alternative' must be either 'less', 'two.sided', or 'greater'")
    # }
    # # make sure var.eq is either true or false
    # if (!is.logical(var.eq)) {
    #   stop("Please specify a logical value for variable 'var.eq'")
    # }
    # # check to make sure additional digit request is numeric
    # if (!is.scalar(more.digits)) {
    #   stop("Argument 'more.digits' must be numeric")
    # }
    # 
    # if (!is.scalar(null.hypoth)){
    # #if (!is.numeric(null.hypoth) ||  length(null.hypoth) > 1 || !is.finite(null.hypoth)){
    #   stop("Null must be a scalar.")
    # }
    # if (!(is.scalar(conf.level) && (conf.level > 0) && (conf.level < 1))){
    #   stop("'conf.level' must be a single number between 0 and 1")
    # } 
    more.digits <- max(floor(more.digits), 0)
    digits <- 3 + more.digits
    cl <- conf.level+(1-conf.level)/2
    if(is.na(n2)){
      twosamp <- FALSE
      est1 <- x1/n1
      se1 <- sqrt(est1 * (1-est1)/n1)
      if (exact){
        test <- stats::binom.test(x1, n1, p = null.hypoth, alternative = alternative, conf.level = conf.level)
        zstat <- NULL
        pval <- as.numeric(format(test$p.value, 
                                  digits = digits))
        cil <- as.numeric(format(min(test$conf.int), 
                                 digits = digits))
        cih <- as.numeric(format(max(test$conf.int), 
                                 digits = digits))

      } else{
        test <- stats::prop.test(x1, n1, p = null.hypoth, alternative = alternative, conf.level = conf.level)
        zstat <- as.numeric(format(sqrt(test$statistic), 
                                   digits = digits))
        pval <- as.numeric(format(test$p.value, 
                                  digits = digits))
        cil <- as.numeric(format(min(test$conf.int), 
                                 digits = digits))
        cih <- as.numeric(format(max(test$conf.int), 
                                 digits = digits))
      }
      est1 <- as.numeric(format(est1, digits = digits))
      se1 <- as.numeric(format(se1, digits = digits))
      printMat <- matrix(c("var1", n1, 
                       est1, se1, paste("[", cil, ", ", cih, 
                                       "]", sep = "")), ncol = 5)
      colnames(printMat) <- c("Variable", "Obs", "Mean", "Std. Error", paste0(conf.level*100, "% CI"))
      rownames(printMat) <- ""
    } else{
      twosamp <- TRUE
      est <- c(x1/n1, x2/n2, x1/n1- x2/n2)
      se <- c(sqrt(est[1] * (1-est[1])/n1), sqrt(est[2] * (1-est[2])/n2), sqrt(est[1] * (1-est[1])/n1 + est[2] * (1-est[2])/n2))
      test <- stats::prop.test(c(x1,x2),c(n1,n2), alternative = alternative, conf.level = conf.level)
      zstat <- as.numeric(format(sqrt(test$statistic), 
                                 digits = digits))
      pval <- as.numeric(format(test$p.value, 
                                digits = digits))
      cil <- c(est[1] - stats::qnorm(cl) * se[1],
               est[2] - stats::qnorm(cl) * se[2],
               min(test$conf.int))
      cih <- c(est[1] + stats::qnorm(cl) * se[1],
               est[2] + stats::qnorm(cl) * se[2],
               max(test$conf.int))
      cil <- as.numeric(format(cil, 
                               digits = digits))
      cih <- as.numeric(format(cih, 
                               digits = digits))
      est <- as.numeric(format(est, digits = digits))
      se <- as.numeric(format(se, digits = digits))
      printMat <- matrix(c("var1", n1, 
                           est[1], se[1], paste("[", cil[1], ", ", cih[2], 
                                            "]", sep = ""),
                           "var2", n2, est[2], se[2], paste("[", cil[2], ", ", cih[2], 
                                                           "]", sep = ""),
                           "diff", n1 + n2, est[3], se[3], paste("[", cil[3], ", ", cih[3], 
                                                                 "]", sep = "")), ncol = 5, byrow = TRUE)
      printMat <- data.frame(printMat)
      names(printMat) <- c("Group", "Obs", 
                       "Mean", "Std. Err.", paste(conf.level * 100, "% CI", sep = ""))
      row.names(printMat) <- c("", " ", "  ")
    }
    par <- c(null.hypoth = null.hypoth, alternative = alternative, 
             conf.level = conf.level, exact = exact,  twosamp = twosamp,
             digits = digits)
    invisible(list(tab = printMat, pval = pval, zstat = zstat, 
                   par = par))
  }
  myargs <- c(deparse(substitute(n1)), deparse(substitute(n2)))
  proptesti.obj <- proptesti.do(x1 = x1, n1 = n1, x2 = x2, n2 = n2,
                          null.hypoth = null.hypoth, conf.level = conf.level,
                          alternative = alternative, eaxct = exact, more.digits = more.digits)
  proptesti.obj$call <- match.call()
  class(proptesti.obj) <- "proptesti"
  return(proptesti.obj)
} 
