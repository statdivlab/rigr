dummy <-
function(x,subset=rep(T,length(x)),reference=sort(unique(x[!is.na(x)])),includeAll=F, version=F) {

    vrsn <- "20150519"
	if (version) return(vrsn)
	cl <- match.call()
	nm <- deparse(cl[[2]])
	if (length(reference)==1) reference <- unique(c(reference,sort(unique(x[!is.na(x)]))))
	X <- NULL
	if (includeAll) X <- cbind(subset*as.integer(x==reference[1])) else X <- NULL
	for (r in reference[-1]) X <- cbind(X,subset*as.integer(x==r))
	if (includeAll) dimnames(X) <- list(NULL,reference)
	else if (length(reference) > 2) {
		dimnames(X) <- list(NULL,paste(reference[-1]," vs ",reference[1],sep=""))
	} else dimnames(X) <- list(NULL,reference[-1])
	attr(X,"transformation") <- "dummy"
	attr(X,"reference") <- reference
  attr(X, "name") <- paste("dummy(", nm, ")", sep="")
	attr(X, "prnm") <- nm
	attr(X,"original") <- x
	X
}
