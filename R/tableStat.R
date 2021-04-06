## Function to create a table of descriptive statistics
## Args: variable            - the variable to make the table on
##       ...                 - arbitrary list of other variables
##       stat                - the statistic to calculate
##       printer             - print or not?
##       na.rm               - how to deal with NAs
##       subset              - subset the variable?
##       probs               - probabilities to display
##       replaceZeroes       - replace zeroes? with what?
##       restriction         - restrict?
##       above,below,labove  - put in an interval
##       rbelow,lbetween,
##       rbetween
##       interval,linterval, - put in an interval
##       rinterval,lrinterval
##       version             - return only the version number?
## Returns: a table of descriptive statistics
## Version: 2015 05 25
tableStat <- function(variable=NULL, ..., stat="count", printer=TRUE, na.rm=TRUE,subset=NULL, probs= c(.25,.50,.75), replaceZeroes=FALSE, restriction=Inf, above=NULL, below=NULL, labove=NULL, rbelow=NULL, lbetween=NULL, rbetween=NULL, interval=NULL, linterval=NULL, rinterval=NULL, lrinterval=NULL, version=FALSE){
  
      
  tableStat.do <- function (variable, ..., stat, printer, na.rm,
                            subset, probs, replaceZeroes, restriction, 
                            above, below, labove, rbelow, lbetween, rbetween, 
                            interval, linterval, rinterval, lrinterval,version) {
    
    vrsn <- "20140715"
    if (version) return(vrsn)
    
    #
    # Provides descriptive statistics (1 at a time) tabulated within strata
    #     variable is a numeric vector (factor is coerced to numeric) or Surv object
    #     ... are the stratifying variables each of which is a vector
    #     stat is one or more (partial matches work) of 
    #           c("count", "missing", "mean", "geometric mean", "median", "sd", 
    #              "variance", "minimum", "maximum", "quantiles", "probabilities", 
    #              "mn(sd)", "range", "iqr","all")
    #           or any formatting string allowed in print.tableStat()
    #     na.rm indicates whether descriptive statistics should only be displayed on
    #           nonmissing values of variable
    #
    # All other arguments are passed through to descrip()
    #
    # The object returned corresponds to stat="all", na.rm=T. Other
    # choices of those arguments just affect the print method. Hence, print.tableStat() can
    # use alternative choices of those arguments.
    #       
    cl <- match.call()
    
    ## gets the strata variables, cleans up in case they came from tabulate
    # if(!is.list(...)){
    #   strata <- list(...)
    #   if(length(strata) > 10){
    #     if(sum(is.na(strata)) > 0){
    #       if(length(which(is.na(strata))) < 2){
    #         strataLen <- which(is.na(strata)) - 2
    #         names <- strata[[which(is.na(strata)) - 1]]
    #       } else {
    #         strataLen <- which(is.na(strata))[2] - 2
    #         if(!is.na(strata[[which(is.na(strata))[1]]])[1]){
    #           names <- strata[[which(is.na(strata))[1]]]
    #         }
    #       }
    #     }
    #     strata <- strata[c(1:strataLen)]
    #   }
    # } else {
    #   strata <- as.vector(...)
    # }
    strata <- list(...)
    if(length(strata) == 1 & is.list(strata[[1]])){
      strata <- strata[[1]]
    } else {
      strataList <- tabModel(strata)
      strata <- strataList$X
      names(strata) <- strataList$preds
    }
    p <- length(strata)
    if (p==0) {
      if (is.null(variable)) stop("must specify variable or strata")
      marginOnly <- T
      if (!is.Surv(variable)) strata <- list(rep(1,length(variable)))
      else strata <- list(rep(1,dim(variable)[1]))
      namesL <- ""
      p <- 1
    } else {
      marginOnly <- F
      namesL <- unlist(match.call(expand.dots=F)$...)
      hyperNames <- as.character(names(match.call(expand.dots=F)$...))
      if(!is.null(names(strata))){
        namesL <- names(strata)
        hyperNames <- attr(strata, "hyperNames")
      }
      if (!is.null(hyperNames) & length(hyperNames) > 0) namesL[hyperNames!=""] <- hyperNames[hyperNames!=""]
      
    }
    n <- length(strata[[1]])
    if (is.null(variable)) {
      nullVariable <- T
      variable <- rep(1,n)
      isDate <- F
    } else {
      nullVariable <- F
      isDate <- inherits(variable,"Date")
      if (isDate) variable <- as.integer(variable)
    }
    if (is.factor(variable)) variable <- as.numeric(variable)
    dimTable <- NULL
    dimLabels <- NULL
    indicators <- NULL
    for (j in 1:p) {
      if (is.factor(strata[[j]])) {
        strata[[j]] <- as.numeric(strata[[j]])
      }
      if (!is.vector(strata[[j]])) stop("stratification arguments must be vectors")
      if (length(strata[[j]]) != n) stop("all stratification arguments must be of same length")
      tmp <- dummy(strata[[j]],includeAll=T)
      if (any(is.na(strata[[j]]))) {
        tmp <- cbind(tmp,"NA"=is.na(strata[[j]]),NotNA=!is.na(strata[[j]]))
      }
      tmp[is.na(tmp)] <- 0
      tmp <- cbind(tmp,ALL=1)
      dimTable <- c(dimTable,dim(tmp)[2])
      dimLabels <- c(dimLabels,list(paste(namesL[j],".",dimnames(tmp)[[2]],sep="")))
      if (is.null(indicators)) {
        indicators <- tmp
      } else {
        indicators <- rep(indicators,dim(tmp)[2]) * matrix(rep(t(tmp),each=dim(indicators)[2]),n,byrow=T)
      }
    }
    indicators[indicators==0] <- NA
    if (!is.Surv(variable)) {
      rslt <- descrip (indicators*variable, probs=probs, geomInclude=TRUE, replaceZeroes=replaceZeroes, restriction=restriction, 
                       above=above, below=below, labove=labove, rbelow=rbelow, lbetween=lbetween, rbetween=rbetween, 
                       interval=interval, linterval=linterval, rinterval=rinterval, lrinterval=lrinterval, subset=subset)
    } else {
      L <- NULL
      for (i in 1:(dim(indicators)[2])) {
        L <- c(L,list(variable))
        L[[i]][,1] <- L[[i]][,1] * indicators[,i]
      }
      names(L) <- dimnames(indicators)[[2]]
      rslt <- descrip (L, probs=probs, geomInclude = TRUE, replaceZeroes=replaceZeroes, restriction=restriction, 
                       above=above, below=below, labove=labove, rbelow=rbelow, lbetween=lbetween, rbetween=rbetween, 
                       interval=interval, linterval=linterval, rinterval=rinterval, lrinterval=lrinterval, subset=subset)
    }
    if (!is.null(subset)) {
      msng <- apply(indicators[subset,,drop=FALSE] * is.na(variable)[subset],2,sum,na.rm=T)
      cnt <- apply(indicators[subset,,drop=FALSE] * rep(1,n)[subset],2,sum,na.rm=T)
    } else {
      msng <- apply(indicators * is.na(variable),2,sum,na.rm=T)
      cnt <- apply(indicators * rep(1,n),2,sum,na.rm=T)
    }
    rslt[,1] <- as.vector(cnt)
    rslt[,2] <- as.vector(msng)
    Lrslt <- NULL
    for (i in 1:(dim(rslt)[2])) {
      Lrslt <- c(Lrslt,list(array(rslt[,i],dim=dimTable,dimnames=dimLabels)))
    }
    for (i in 3:length(Lrslt)) Lrslt[[i]][cnt==0] <- NaN
    if (nullVariable) for (i in 3:length(Lrslt)) Lrslt[[i]] <- Lrslt[[i]] * NA
    names(Lrslt) <- dimnames(rslt)[[2]]
    attr(Lrslt,"stat") <- stat
    attr(Lrslt,"call") <- cl
    attr(Lrslt,"na.rm") <- na.rm
    attr(Lrslt,"marginOnly") <- marginOnly
    attr(Lrslt,"isDate") <- isDate
    attr(Lrslt, "printer") <- printer
    class(Lrslt) <- "tableStat"
    Lrslt
  }
  strata <- list(...)
  if(length(strata) == 1){
    if(is.list(strata[[1]])) {
      strata <- strata[[1]]
    }
  } 
  if(length(names(strata)) < 1){
    names(strata) <- as.character(unlist(match.call(expand.dots=F)$...))
  }
  tableStat.obj <- tableStat.do(variable=variable, strata, 
                                stat=stat, printer=printer, na.rm=na.rm,subset=subset, probs=probs,  
                                replaceZeroes=replaceZeroes, restriction=restriction, 
                                above=above, below=below, labove=labove, rbelow=rbelow, lbetween=lbetween, 
                                rbetween=rbetween, interval=interval, linterval=linterval, rinterval=rinterval, 
                                lrinterval=lrinterval, version=version)
  tableStat.obj$call <- match.call()
  attr(tableStat.obj, "call") <- match.call()
  class(tableStat.obj) <- "tableStat"
  return(tableStat.obj)
}
