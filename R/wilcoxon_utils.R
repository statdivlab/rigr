# Print method for class wilcoxon
#' @noRd
#' @export
print.wilcoxon <-
  function(x,...){
    out <- x
    cat("\n", out$method, "\n")
    print(out$table)
    print(out$vars)
    print(out$hyps, quote=FALSE)
    print(out$inf, quote=FALSE)
  }
