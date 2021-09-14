#' Create Polynomials
#' 
#' Creates polynomial variables, to be used in regression. Will create polynomials of degree less than
#' or equal to the \code{degree} specified, and will mean center variables by default.
#' 
#' 
#' @param x variable used to create the
#' polynomials.
#' @param degree the maximum degree
#' polynomial to be returned. Polynomials of degree <= \code{degree} will be
#' returned.
#' @param center the value to center the polynomials at.
#' @return A matrix containing the linear
#' splines. 
#' @examples
#' 
#' # Reading in a dataset
#' data(mri)
#' 
#' # Create a polynomial on ldl
#' polynomial(mri$ldl, degree=3)
#' 
#' # Use a polynomial in regress
#' regress("mean", atrophy ~ polynomial(age, degree = 2), data = mri)
#' 
#' @export polynomial
polynomial <- function(x,degree=2,center=mean(x,na.rm=TRUE)) {
  cl <- match.call()
  nm <- deparse(cl[[2]])
  x <- x - center
  if (length(degree)==1) {
    degree <- round(degree)
    if (degree < 2) stop("inappropriate degree of polynomial")
    nms <- paste(c("Linear","Square","Cube"),ifelse1(center!=0,"(ctr)",""),sep="")[1:min(degree,3)]
    if (degree > 3) nms <- c(nms,paste(4:degree,"thPwr",ifelse1(center!=0,"(ctr)",""),sep=""))
    degree <- 1:degree
  } else {
    stop("polynomial degree must be a single number")
  }
  rslt <- NULL
  for (d in degree) rslt <- cbind(rslt,x^d)
  dimnames(rslt) <- list(NULL,nms)
  attr(rslt,"transformation") <- "polynomial"
  attr(rslt, "name") <- paste("polynomial(", nm, ")", sep="")
  attr(rslt, "prnm") <- nm
  attr(rslt,"degree") <- degree
  attr(rslt,"center") <- center
  attr(rslt,"original") <- x
  rslt
}
