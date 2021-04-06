print.augCoefficients <-
function (x,...,sigfigs=max(3,getOption("digits")-3),width=9,nonsci.limit=5,Psci=FALSE, version=FALSE) {

    vrsn <- "20110928"
	if (version) return(vrsn)
		

	#
	# prints CI or exp(CI) to specified sigfigs
	# rounds exp(Est) to same number of decimal figures as exp(CI)
	# prints Std Err or Robust SE to specified sigfigs
	# rounds Estimate to same number of decimal figures as StdErr or Robust SE
	# prints F stat to specified sigfigs
	# prints P value to number of decimal figures as nonsci.limit unless < 10^-nonsci.limit when "< .00001" used
	# centers all but df and P value, which is right justified
	
	cmptRoundDigits <- function (x, sf) {
		y <- max(abs(x),na.rm=T)
		if (y==0) {
			sf
		} else {
			y <- trunc(log(y) / log(10)) - (y < 1)
			max(0,sf - y - (y < sf))
		}
	}
		
	frmtCol <- function (x, sf, nonsci.limit, colwidth=9) {
		rslt <- NULL
		for (i in 1:length(x)) {	
			if (is.na(x[i])) {
				tmp <- "NA"
			} else {
				rd <- cmptRoundDigits (x[i], sf)
				if (rd <= nonsci.limit & abs(x[i]) < 10^nonsci.limit) {
					tmp <- format(round(x[i],rd),nsmall=rd,width=1)
				} else {
					tmp <- format(round(x[i],rd), digits=sf, scientific=T, width=1)
				}
			}
			rslt <- c(rslt,ifelse(x[i]<0,tmp,paste(" ",tmp,sep="")))
		}
		format(rslt, justify="centre", width=colwidth)
	}
						
	frmtCoefficients <- format(x)
	Pvalcol <- dim(x)[2]
	for (j in 1:(Pvalcol-3)) frmtCoefficients[,j] <- frmtCol (x[,j],sigfigs,nonsci.limit,width)
	SEcol <- ifelse(dimnames(x)[[2]][3]=="RobustSE",3,2)
	frmtCoefficients[,SEcol+1] <- paste(frmtCoefficients[,SEcol+1],"  ")
	frmtCoefficients[,Pvalcol-2] <- paste("  ",format(round(x[,Pvalcol-2],2),width=width,justify="right"))
	dimnames(frmtCoefficients)[[2]][Pvalcol-2] <- paste("  ",dimnames(frmtCoefficients)[[2]][Pvalcol-2])
	frmtCoefficients[,Pvalcol-1] <- format(x[,Pvalcol-1])
	if (Psci) {
		frmtCoefficients[,Pvalcol] <- format(x[,Pvalcol],digits=sigfigs,scientific=T,width=width,justify="centre")
	} else{
		z <- paste(format(round(x[,Pvalcol],4),width=width-1,justify="right")," ",sep="")
		z[x[,Pvalcol] < 5e-5] <- format("< 0.00005",width=width,justify="right")
		frmtCoefficients[,Pvalcol] <- format(z,justify="right",width=width)
	}
	u <- is.na(x[,1])
	if (any(u)) frmtCoefficients[u,1:(Pvalcol-3)] <- ""
	dimnames(frmtCoefficients)[[1]] <- paste(dimnames(frmtCoefficients)[[1]],"  ")
	print(frmtCoefficients,quote=F)
	invisible(frmtCoefficients)
}
