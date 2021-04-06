print.uDescriptives <-
function (x, ..., sigfigs=max(3,getOption("digits")-3),width=9,nonsci.limit=5, print.it= TRUE, version=F) {
  
  vrsn <- "20121026"
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
  
  ncol <- dim(x)[2]
  meancol <- (1:ncol)[dimnames(x)[[2]]=="Mean"]
  mincol <- (1:ncol)[dimnames(x)[[2]]==" Min"]
  maxcol <- (1:ncol)[dimnames(x)[[2]]==" Max"]
  censMin <- x[,"firstEvent"] > x[,mincol]
  censMin[is.na(censMin)] <- F
  censMax <- x[,"lastEvent"] < x[,maxcol]
  censMax[is.na(censMax)] <- F
  if (!any(censMax)) {
    restriction <- NULL
  } else {
    restriction <- frmtCol(x[,"restriction"],sigfigs,nonsci.limit,1,")")
    restriction <- paste("(R",restriction,sep="")
    restriction[!censMax] <- ""
    restriction <- format(restriction, justify="left")
  }
  frmtCoefficients <- format(x[,1:(ncol-4),drop=F])
  for (j in 1:2) frmtCoefficients[,j] <- format (x[,j],justify="right",width=5)
  frmtCoefficients[,mincol] <- frmtCol (x[,mincol],sigfigs,nonsci.limit,width,ifelse(censMin,"+",""))
  for (j in 3:5) frmtCoefficients[,j] <- frmtCol (x[,j],sigfigs,nonsci.limit,width,ifelse(censMax,"+",""))
  if(any(dimnames(x)[[2]] == "Geom Mn")){
    indx <- 7:(ncol-4)
  } else {
    indx <- 6:(ncol-4)
  }
  indx <- indx[indx != maxcol]
  for (j in indx) frmtCoefficients[,j] <- frmtCol (x[,j],sigfigs,nonsci.limit,width)
  frmtCoefficients[,maxcol] <- frmtCol (x[,maxcol],sigfigs,nonsci.limit,width,ifelse(censMax,"+",""))
  ## set a temp variable to check if any are NA
  dateBool <- any(is.na(x[,"isDate"]))
  if(dateBool){
    
  } else {
    if (any(x[,"isDate"]==1)) {
      xformCol <- c(3,5:(dim(x)[2]-4))
      for (j in 1:length(xformCol)) {
        if (substring(dimnames(x)[[2]][xformCol[j]],1,2)=="Pr") xformCol[j] <- NA
      }
      xformCol <- xformCol[!is.na(xformCol)]
      orgn <- "1970-01-01"
      frmtCoefficients[x[,"isDate"]==1,xformCol] <- format(as.Date(x[x[,"isDate"]==1,xformCol],orgn))
    }
  }
  if (!is.null(restriction)) frmtCoefficients <- cbind(frmtCoefficients[,1:2,drop=F],
                                                       "Restrict"=restriction,frmtCoefficients[,-(1:2),drop=F])
  if(print.it) print(frmtCoefficients,quote=F)
  invisible(frmtCoefficients)
}
