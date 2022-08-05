# Print method for class "anova"
#' @noRd
#' @export
print.anova.uRegress <- function(x, ...) {
  
  cat("Analysis of Deviance Table \n")
  cat("Null model: ")
  print(x$null_model)
  cat("\n")
  cat("Full model: ")
  print(x$full_model)
  cat("\n")
  stats::printCoefmat(t(x$printMat[1,]), digits = 4, 
                      has.Pvalue = TRUE)
  
}