#' Create Polynomials
#' 
#' Creates polynomial variables.
#' 
#' 
#' @param x variable used to create the
#' polynomials.
#' @param degree the maximum degree
#' polynomial to be returned. Polynomials of degree <= \code{degree} will be
#' returned.
#' @param center the value to center the polynomials at.
#' @param version if \code{TRUE}, returns the version of the function and
#' nothing else.
#' @return A matrix containing the linear
#' splines. 
#' @examples
#' 
#'   # Reading in a dataset
#'   data(mri)
#'   attach(mri)
#'   # Create a polynomial on ldl
#'   polynomial(ldl, degree=3)
#' 
#' @export polynomial
polynomial <-
function(x,degree=2,center=mean(x,na.rm=T), version=F) {

    vrsn <- "20150519"
	if (version) return(vrsn)
		
cl <- match.call()
nm <- deparse(cl[[2]])
	x <- x - center
	if (length(degree)==1) {
		degree <- round(degree)
		if (degree < 2) stop("inappropriate degree of polynomial")
		nms <- paste(c("Linear","Square","Cube"),ifelse1(center!=0,"(ctr)",""),sep="")[1:min(degree,3)]
		if (degree > 3) nms <- c(nms,paste(4:degree,"thPwr",ifelse1(center!=0,"(ctr)",""),sep=""))
		degree <- 1:degree
	} else nms <- paste("^",format(degree),ifelse1(center!=0,"(ctr)",""),sep="")
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
