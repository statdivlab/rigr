## Function to perform a Wald Test on data
## Args: full - a uRegress object
##  contrasts - the matrix containing which objects from full to test
## hypothesis - the null hypothesis
##    version - which version the function is
## Returns: a wald test on the given coefficients
## Version: 2014 04 27
uWaldtest <-
function (full, contrasts=c(0,rep(1,p-1)), hypothesis=matrix(0,r,1), version=F) {

    vrsn <- "20150427"
	if (version) return(vrsn)
		
	if (!inherits(full,"uRegress")) stop("not a uRegress object")
	coefficients <- full$coefficients[,1]
	p <- length(coefficients)
	if (!is.matrix(contrasts)) contrasts <- matrix(contrasts,1)
	if (dim(contrasts)[2] != p) stop ("contrasts, coefficient vector must be conformable")
	r <- dim(contrasts)[1]
	if (length(hypothesis) != r) stop ("contrasts, hypothesis must be conformable")
	covmtx <- full$robustCov
	if (is.null(covmtx)) covmtx <- full$naiveCov
	covmtx <- contrasts %*% covmtx %*% t(contrasts)
	Fstat <- t(contrasts %*% coefficients - hypothesis) %*% solve(covmtx) %*% (contrasts %*% coefficients - hypothesis) / r
	if (full$useFdstn) {
		df.den <- full$waldStat[4]
		pval <- 1-pf(Fstat,r,df.den)
		rslt <- c(Fstat, pval,r,df.den)
		names(rslt) <- c("F stat","p value","num df","den df")
	} else {
		pval <- 1-pchisq(Fstat,r,)
		rslt <- c(Fstat, pval,r)
		names(rslt) <- c("Chi2 stat","p value","df")
	}
	rslt	
}
