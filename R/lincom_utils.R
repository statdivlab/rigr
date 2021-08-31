# Print method for class "lincom"
#' @noRd
#' @export
print.lincom <- function(x, ...) {
  for (i in 1:(length(x)-1)){
    cat("\nH0:", x[[i]]$nms, "  = ", x[[i]]$hyp, "\n")
    cat("Ha:", x[[i]]$nms, " != ", x[[i]]$hyp, "\n")
    stats::printCoefmat(t(x[[i]]$printMat[1,]), digits = 4, 
                 has.Pvalue = TRUE)
  }
}