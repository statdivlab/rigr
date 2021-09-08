#' Print method for class "ttesti"
#' @noRd
#' @export
print.ttesti <- function(x, ...) {
  out <- x
  cat("\nCall:\n")
  print(out$call)
  main <- out$tab
  df <- out$df
  pval <- out$p
  tstat <- out$tstat
  par <- out$par
  null.hypoth = par[1]
  alternative = par[2]
  var.eq = as.logical(par[3])
  conf.level = par[4]
  twosamp <- as.logical(par[5])
  digits = par[6]
  null.hypoth <- as.numeric(format(null.hypoth, digits = digits))
  if (var.eq) {robust <- FALSE}
  if (!var.eq) {robust <- TRUE}
  if (alternative == "less") {par1 <- c(">=", "<", "Pr(T < t) = ")}
  if (alternative == "two.sided") {par1 <- c("=", "!=", "Pr(|T| > t) = ")}
  if (alternative == "greater") {par1 <- c("<=",">", "Pr(T > t) = ")}
  par2 <- ""
  partest <- ""
  partest2 <- "difference in"
  
  if (twosamp) {
    if (var.eq){
      par2 <- "presuming equal variances"
    } else{
      par2 <- "allowing for unequal variances"
    }
  }
  if (!twosamp){
    inference0 <- paste("Ho: ", partest, " mean ", par1[1], " ", null.hypoth," ;", sep = "")
    inference1 <- paste("Ha: ", partest, " mean ", par1[2], " ", null.hypoth, sep = "")
    inferencet <- paste("t = ", tstat, sep = "")
    inferencedf <- paste("df = ",df, sep = "")
    inferencep <- paste(par1[3],pval)
    cat("\nOne-sample t-test", ":\n", collapse = "\n")
    cat("Summary:\n")
    print(main, quote = FALSE)
    cat("\n",inference0,"\n", inference1,"\n", inferencet,",", inferencedf,"\n",inferencep, collapse = "\n")
  } else{
    inference0 <- paste("Ho: ", partest2, " ", partest, " means ", par1[1], " ", null.hypoth," ;", sep = "")
    inference1 <- paste("Ha: ", partest2, " ", partest, " means ", par1[2], " ", null.hypoth, sep = "")
    inferencet <- paste("t = ", tstat, sep = "")
    inferencedf <- paste("df = ",df, sep = "")
    inferencep <- paste(par1[3],pval)
    cat("\nTwo-sample t-test", par2, ":\n", collapse = "\n")
    cat("Summary:\n")
    print(main, quote = FALSE)
    cat("\n",inference0,"\n", inference1,"\n", inferencet,",", inferencedf,"\n",inferencep, collapse = "\n")
  }
}