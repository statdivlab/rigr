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
