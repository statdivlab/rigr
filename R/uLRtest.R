uLRtest <-
function (full,reduced, version=F) {

    vrsn <- "20110928"
	if (version) return(vrsn)
		
	if (dimnames(full$coefficients)[[2]][2]=="Robust SE")
		stop("uLRtest inappropriate with robust SE")
	if (dimnames(reduced$coefficients)[[2]][2]=="Robust SE") 
		stop("uLRtest inappropriate with robust SE")
	if (!all(dimnames(reduced$coefficients)[[1]] %in% dimnames(reduced$coefficients)[[1]])) 
		stop("uLRtest only appropriate with hierarchical models")
	if (dim(reduced$lmobj$model)[1] != dim(full$lmobj$model)[1]) 
		stop("full and reduced models must be based on same data")
	if (any(dimnames(reduced$lmobj$model)[[1]] != dimnames(full$lmobj$model)[[1]])) 
		stop("full and reduced models must be based on same data")
	
	df.full <- full$df[2]
	df.redu <- reduced$df[2]
	RSSH <- reduced$sigma^2 * df.redu
	RSS <- full$sigma^2 * df.full
	Fstat <- (RSSH - RSS) / (df.redu - df.full) / (full$sigma^2)
	pval <- 1-pf(Fstat,df.redu-df.full,df.full)
	rslt <- c(Fstat, pval,df.redu-df.full,df.full)
	names(rslt) <- c("F stat","p value","num df","den df")
	rslt
	}
