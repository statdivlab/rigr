#' Create Dummy Variables
#' 
#' 
#' @param x \code{y} variable used to create the dummy
#' variables.
#' @param subset \code{cluster} a subset of the data, if
#' desired.
#' @param reference the reference value for the dummy variables to compare to.
#' @param includeAll logical value indicating whether all of the dummy
#' variables should be returned (including the reference).
#' @return A matrix containing the dummy
#' variables.
#' @examples
#' 
#' data(mri)
#' 
#' # Create a dummy variable for chd
#' dummy(mri$chd)
#' 
#' @export dummy
dummy <- function(x, subset=rep(TRUE,length(x)), reference=sort(unique(x[!is.na(x)])),includeAll=FALSE) {
	cl <- match.call()
	nm <- deparse(cl[[2]])
	
	reference_name <- reference
	
	if (length(reference)==1) {
	  reference <- unique(c(reference,sort(unique(x[!is.na(x)]))))
	} 
	
	X <- NULL
	
	if (includeAll) {
	  X <- cbind(subset*as.integer(x==reference[1])) 
	} 
	
	for (r in reference[-1]) {
	  X <- cbind(X, subset*as.integer(x == r))
	}
	
	if (includeAll) {
	  dimnames(X) <- list(NULL,reference)
	} else if (length(reference) > 2) {
		dimnames(X) <- list(NULL,paste(reference[-1]," vs ",reference[1],sep=""))
	} else {
	  dimnames(X) <- list(NULL,paste("dummy(", nm, ")." ,reference[-1]," vs ",reference[1],sep=""))
	} 
	
	attr(X,"transformation") <- "dummy"
	attr(X,"reference") <- reference
  attr(X, "name") <- paste("dummy(", nm, ")", sep="")
	attr(X, "prnm") <- nm
	attr(X,"original") <- x
	attr(X, "groups") <- sort(unique(x))
	attr(X, "labels") <- dimnames(X)
	attr(X, "reference_name") <- reference_name
	X
}
