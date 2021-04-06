print.wilcoxon <-
function(x,...){
  out <- x
  cat("\n", out$method, "\n")
  print(out$table)
  print(out$vars)
  print(out$hyps, quote=FALSE)
  print(out$inf, quote=FALSE)
  if(dim(out$inf)[2] == 4){
    warning("The 'confidence interval' presented is semi-parametric and non-robust, and is a test of medians.")
  }
}
