fitted.uRegress <-
function (object,...,X, version=FALSE) {

  obj <- object
    vrsn <- "20110928"
	if (version) return(vrsn)
		

	if (!missing(X)) {
		if (dim(X)[2] != dim(obj$processTerms$X)[2]) stop("new data not appropriate dimensions")
		if (dim(obj$coefficients)[1] == dim(X)[2] + 1) X <- cbind(1,X)
		fits <- X %*% obj$coefficients[,1]
	} else fits <- obj$glmobj$fitted.values
	if (obj$functional == "geometric mean") fits <- exp(fits)
	fits
}
