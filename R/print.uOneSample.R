print.uOneSample <-
function (x, ...,sigfigs=max(3,getOption("digits")-3),width=9,nonsci.limit=5, print.it= TRUE, version=F) {
  
  vrsn <- "20121107"
  if (version) return(vrsn)
  obj <- x
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
  
  frmtCol <- function (x, sf, nonsci.limit, colwidth=9, append="") {
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
    rslt <- paste(rslt,append,sep="")
    format(rslt, justify="centre", width=colwidth)
  }
  
  x <- obj$Inference
  ncol <- dim(x)[2]
  frmtCoefficients <- format(x)
  frmtCoefficients[,1] <- format (x[,1],width=5)
  for (j in 2:ncol) frmtCoefficients[,j] <- frmtCol (x[,j],sigfigs,nonsci.limit,width)
  if (attr(x,"isDate") && attr(x,"fnctl") %in% c("mean","geometric mean","median","quantile")) {
    xformCol <- 2:4
    orgn <- "1970-01-01"
    frmtCoefficients[x[,"isDate"]==1,xformCol] <- format(as.Date(x[x[,"isDate"]==1,xformCol],orgn))
  }
  dimnames(frmtCoefficients)[[2]] <- format(dimnames(x)[[2]],justify="centre")
  restriction <- attr(x,"restriction")
  if (restriction < Inf) frmtCoefficients <- cbind(frmtCoefficients[,1,drop=F],
                                                   "Restrict"=restriction,frmtCoefficients[,-1,drop=F])
  if(print.it) {
    if (!is.null(attr(x,"hName"))) cat(attr(x,"hName"),"\n")
    cat("Method:",attr(x,"mName"),"\n")
    methodParams <- attr(x,"methodParams")
    if(!is.null(methodParams)) {
      cat("      Parameters:\n")
      print(methodParams)
    }
    nMsng <- attr(x, "nMsng")
    if(!is.na(nMsng) && nMsng > 0) cat(nMsng, "observations deleted due to missing values\n")
    print(frmtCoefficients,quote=F)
  }
  invisible(frmtCoefficients)
}
