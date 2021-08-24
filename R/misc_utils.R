#' Error handling - scalar
#' @noRd

is.scalar <- function(x){
  result <- (is.numeric(x) && length(x) == 1 && is.finite(x))
  return(result)
}

#' non-vectorized ifelse function
#' @noRd
ifelse1 <-
  function (test, x, y) if (test) x else y
