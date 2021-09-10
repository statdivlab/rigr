#' Test of proportions from summary statistics
#' 
#' Performs a one- or two-sample test of proportions using counts of successes and trials, rather than
#' binary data. This test can be approximate or exact. 
#' 
#' If \code{x2} or \code{n2} are specified, then both must be specified, and a two-sample test is run.
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
#' two-sample test). Defaults to 0.5 for one-sample and 0 for two-sample.
#' @param alternative a string: one of
#' \code{"less"}, \code{"two.sided"}, or \code{"greater"} specifying the form
#' of the test. Defaults to a two-sided test.
#' @param conf.level confidence level of the
#' test. Defaults to 0.95
#' @param correct a logical indicating whether to perform a continuity correction
#' @param more.digits a numeric value
#' specifying whether or not to display more or fewer digits in the output.
#' Non-integers are automatically rounded down.
#' 
#' @return A list of class \code{proptesti}. The print method lays out the information in an easy-to-read
#' format. 
#' \item{tab}{A formatted table of descriptive and inferential results (total number of observations,
#' sample proportion, standard error of the proportion estimate), 
#' along with a confidence interval for the underlying proportion.}
#' \item{zstat}{the value of the test
#' statistic, if using an approximate test.} 
#' \item{pval}{the p-value
#' for the test} 
#' \item{par}{A vector of information about the type of test (null hypothesis, alternative hypothesis, etc.)}
#' 
#' @examples
#' # Two-sample test
#' proptesti(10, 100, 15, 200, alternative = "less")
#' 
#' @export proptesti
proptesti <- function(x1, n1, x2 = NULL, n2 = NULL, exact = FALSE,
                      null.hypoth = ifelse(is.null(x2) && is.null(n2), 0.5, 0), 
                      conf.level=.95, 
                      alternative="two.sided", 
                      correct = FALSE,
                      more.digits = 0){
  proptesti.do <- function(x1, n1, x2 = NULL, n2 = NULL, exact = FALSE,
                           null.hypoth = ifelse(is.null(x2) && is.null(n2), 0.5, 0), 
                           conf.level=.95, 
                           alternative="two.sided", 
                           correct = FALSE,
                           more.digits = 0, ...) {
    if (!(is.scalar(x1) && is.scalar(n1) && x1%%1 == 0 && n1%%1 == 0 && x1 >= 0 && n1 > 0)){
      stop("'x1' and 'n1' must be nonnegative integers")
    }
    if (x1 > n1){
      stop("Number of trials must be at least as large as number of succeses.")
    }
    if (!is.null(x2) && !is.null(n2)){
      if (!(is.scalar(x2) && is.scalar(n2) && x2%%1 == 0 && n2 %%1 == 0 && x2 >= 0 && n2 > 0)){
        stop("'x2' and 'n2' must be nonnegative integers")
      }
      if (x2 > n2){
        stop("Number of trials must be at least as large as number of succeses.")
      }
    }
    if(is.null(n2) && !is.null(x2)) {
      stop("A second number of trials must be entered for two sample test")
    }
    if(!is.null(n2) && is.null(x2)){
      stop("Number of successes for the second sample must be entered for two sample test")
    }
    # check that alternative is one of "less", "two.sided", or "greater"
    if (!(alternative %in% c("less", "two.sided", "greater"))) {
      stop("'alternative' must be either 'less', 'two.sided', or 'greater'")
    }
    if (!(is.scalar(null.hypoth))){
      stop("Null must be a scalar")
    }
    if (!is.scalar(conf.level) || conf.level < 0 || conf.level > 1){
      stop("'conf.level' must a scalar between 0 and 1.")
    }
    if (!is.logical(exact)){
      stop("'exact' must be a logical.")
    }
    if (!is.logical(correct)){
      stop("'correct' must be a logical.")
    }
    if (!is.null(n2) && exact){
      stop("Exact binomial test not available for two samples.")  
    }
    if (null.hypoth != 0 && !is.null(n2)){
      stop("Two sample test only allows a null of 0.")
    }
    # check to make sure additional digit request is numeric
    if (!is.numeric(more.digits)) {
      stop("Argument 'more.digits' must be numeric")
    }
    more.digits <- max(floor(more.digits), 0)
    digits <- 3 + more.digits
    cl <- conf.level+(1-conf.level)/2
    if(is.null(n2)){
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
        test <- stats::prop.test(x1, n1, p = null.hypoth, alternative = alternative, conf.level = conf.level, correct = correct)
        zstat <- as.numeric(format(sign(est1 - null.hypoth)*sqrt(test$statistic), 
                                   digits = digits))
        pval <- as.numeric(format(test$p.value, 
                                  digits = digits))
        cil <- as.numeric(format(est1 - stats::qnorm(cl)*se1, 
                                 digits = digits))
        cih <- as.numeric(format(est1 + stats::qnorm(cl)*se1, 
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
      test <- stats::prop.test(c(x1,x2),c(n1,n2), alternative = alternative, conf.level = conf.level, correct = correct)
      zstat <- as.numeric(format(sign(est[3])*sqrt(test$statistic), 
                                 digits = digits))
      pval <- as.numeric(format(test$p.value, 
                                digits = digits))
      cil <- c(est[1] - stats::qnorm(cl) * se[1],
               est[2] - stats::qnorm(cl) * se[2],
               est[1] - est[2] - stats::qnorm(cl)*se[3])
      cih <- c(est[1] + stats::qnorm(cl) * se[1],
               est[2] + stats::qnorm(cl) * se[2],
               est[1] - est[2] + stats::qnorm(cl)*se[3])
      cil <- as.numeric(format(cil, 
                               digits = digits))
      cih <- as.numeric(format(cih, 
                               digits = digits))
      est <- as.numeric(format(est, digits = digits))
      se <- as.numeric(format(se, digits = digits))
      printMat <- matrix(c("var1", n1, 
                           est[1], se[1], paste("[", cil[1], ", ", cih[1], 
                                                "]", sep = ""),
                           "var2", n2, est[2], se[2], paste("[", cil[2], ", ", cih[2], 
                                                            "]", sep = ""),
                           "Difference", n1 + n2, est[3], se[3], paste("[", cil[3], ", ", cih[3], 
                                                                       "]", sep = "")), ncol = 5, byrow = TRUE)
      printMat <- data.frame(printMat)
      names(printMat) <- c("Group", "Obs", 
                           "Mean", "Std. Err.", paste(conf.level * 100, "% CI", sep = ""))
      row.names(printMat) <- c("", " ", "  ")
    }
    par <- c(null.hypoth = null.hypoth, alternative = alternative, 
             conf.level = conf.level, exact = exact,  twosamp = twosamp,
             correct = correct, digits = digits)
    invisible(list(tab = printMat, pval = pval, zstat = zstat, 
                   par = par))
  }
  myargs <- c(deparse(substitute(n1)), deparse(substitute(n2)))
  proptesti.obj <- proptesti.do(x1 = x1, n1 = n1, x2 = x2, n2 = n2,
                                null.hypoth = null.hypoth, conf.level = conf.level,
                                alternative = alternative, exact = exact, correct = correct, more.digits = more.digits)
  proptesti.obj$call <- match.call()
  class(proptesti.obj) <- "proptesti"
  return(proptesti.obj)
} 
