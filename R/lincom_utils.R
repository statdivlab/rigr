# Print method for class "lincom"
#' @noRd
#' @export
print.lincom <- function(x, ...) {
  for (i in 1:(length(x)-1)){
    cat("\nH0:", x[[i]]$nms, "  = ", x[[i]]$null.hypoth, "\n")
    cat("Ha:", x[[i]]$nms, " != ", x[[i]]$null.hypoth, "\n")
    stats::printCoefmat(t(x[[i]]$printMat[1,]), digits = 4, 
                 has.Pvalue = TRUE)
  }
}

# Print method for class "lincom" when joint.test=TRUE
#' @noRd
#' @export
print.lincom.joint <- function(x, ...) {
  stats::printCoefmat(t(x$printMat[1,]), digits = 4, 
                        has.Pvalue = TRUE)
}