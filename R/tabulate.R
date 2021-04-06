## Function to perform a "tabulate" like STATA
## Args: ...        - arbitrary list of variables
##       dispRatios - display the OR/RR?
##       stratified - should the table be stratified?
##       tests      - what tests to display
##       the rest   - args to tableStat
## Returns: a table with the specified values
## Version: 2015 05 25
tabulate <- function(..., dispRatios=FALSE, stratified=TRUE, tests=NULL, stat="count", na.rm=TRUE,
                     subset=NULL, probs= c(.25,.50,.75), replaceZeroes=FALSE, restriction=Inf, 
                     above=NULL, below=NULL, labove=NULL, rbelow=NULL, lbetween=NULL, rbetween=NULL, 
                     interval=NULL, linterval=NULL, rinterval=NULL, lrinterval=NULL, version=FALSE){
  tabulate.do <- function(..., dispRatios, stratified, tests, stat, na.rm,
                          subset, probs, replaceZeroes, restriction, 
                          above, below, labove, rbelow, lbetween, rbetween, 
                          interval, linterval, rinterval, lrinterval, version){
    if(version){return("20141002")}
    ## capture the elements in ...
    strata <- list(...)
    if(length(strata) == 1 & is.list(strata[[1]])) {
      strata <- strata[[1]]
    } else {
      strataList <- tabModel(strata)
      strata <- strataList$X
      names(strata) <- strataList$preds
    }
    if(length(strata) > 1){
      if(dispRatios == TRUE & (length(levels(as.factor(strata[[1]]))) != 2 | length(levels(as.factor(strata[[2]]))) != 2)){
        stop("First two variables must take on only two values")
      }
    }
    ## make sure that the data meet the criteria for the mh test if it is requested
    if("mh" %in% tests){
      if(length(strata) < 3){
        stop("Three variables must be entered for M-H test")
      }
      if (length(levels(as.factor(strata[[1]]))) < 2 | length(levels(as.factor(strata[[2]]))) < 2 | length(levels(as.factor(strata[[3]]))) < 2){
        stop("First three variables must take on at least 2 values for M-H test")
      }
    } 

    ## run table stat to get count tables
    tab <- tableStat(variable=NULL, strata, stat=stat, printer=FALSE, na.rm=na.rm, 
                     subset=subset, probs=probs,
                     replaceZeroes=replaceZeroes, restriction=restriction, above=above, below=below, 
                     labove=labove, rbelow=rbelow, lbetween=lbetween, rbetween=rbetween, 
                     interval=interval, linterval=linterval, rinterval=rinterval, version=version)
    ## get other descriptives (row%, col%, etc) if present
    printTab <- print.tableStat(tab)
    ## makes sure that the printer table is correct on default
    if(is.null(dim(printTab))){
      printTab <- printTab$count
    }
    #}
    ## create linear model for score, likelihood ratio, and wald tests
    linMods <- list()
    if("wald" %in% tests | "lr" %in% tests | "score" %in% tests){
      strataOne <- cbind(NULL, strata[[1]])
      strataArray <- NULL
      colnames(strataOne) <- names(strata[1])
      if(length(levels(as.factor(strataOne))) == 2){
        linMods[[1]] <- glm(strataOne~1, family=binomial)
      } else {
        linMods[[1]] <- glm(strataOne~1, family=poisson)
      }
      for(i in 2:length(strata)){
        strataArray <- cbind(strataArray, strata[[i]])
        if(i == 2){
          colnames(strataArray) <- c(colnames(strataArray), names(strata)[i])
        } else {
          colnames(strataArray)[i-1] <- names(strata)[i]
        }
        if(length(levels(as.factor(strataOne))) == 2){
          linMods[[i]] <- glm(strataOne~., family = binomial, data=as.data.frame(strataArray))
        } else {
          linMods[[i]] <- glm(strataOne~., family=poisson, data=as.data.frame(strataArray))
        }
      }
    }
    invisible(list(rslt = tab, printer = printTab, tests=tests, dispRatios=dispRatios, linMods=linMods, names=names(strata), stratified=stratified, stat=stat))
    
  }
  strata <- list(...)
  names(strata) <- as.character(unlist(match.call(expand.dots=F)$...))
  hyperNames <- as.character(names(match.call(expand.dots=F)$...))
  attr(strata, "hyperNames") <- hyperNames
  if(length(hyperNames) == 0) hyperNames <- NULL
  if (!is.null(hyperNames)) names[hyperNames!=""] <- hyperNames[hyperNames!=""]
  tabulate.obj <- tabulate.do(strata, dispRatios=dispRatios, stratified=stratified, tests=tests, stat=stat, na.rm=na.rm,
                              subset=subset, probs=probs, replaceZeroes=replaceZeroes, restriction=restriction, 
                              above=above, below=below, labove=labove, rbelow=rbelow, lbetween=lbetween, rbetween=rbetween,
                              interval=interval, linterval=linterval, rinterval=rinterval, lrinterval=lrinterval, version=version)
  tabulate.obj$call <- match.call()
  class(tabulate.obj) <- "tabulate"
  tabulate.obj
}
