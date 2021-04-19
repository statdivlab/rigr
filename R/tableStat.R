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




#' Table of Stratified Descriptive Statistics
#' 
#' Produces a table of stratified descriptive statistics for a single variable
#' of class \code{integer}, \code{numeric}, \code{Surv}, \code{Date}, or
#' \code{factor}. Descriptive statistics are those that can be estimated using
#' the \code{\link[uwIntroStats]{descrip}} function.
#' 
#' This function
#' uses \code{descrip()} to compute the descriptive statistics. In addition to
#' the basic choices specified above for \code{stat}, the user can supply a
#' special format character string. Arbitrary text can be specified to label
#' any of the descriptive statistics, which are indicated by bracketing with a
#' "\\@". All text bracketed by a ``\\@'' must refer to a descriptive statistic,
#' and all other text is printed verbatim. For instance, a display of the mean,
#' standard deviation, minimum, maximum, and sample size might be specified by
#' ``\\@mean\\@ (\\@sd\\@; \\@min\\@ - \\@max\\@; n=\\@count\\@)''. Similarly, a cross tabulation
#' displaying counts, row percentages, column percentages, and percentages of
#' the total might be specified by ``\\@count\\@ (r \\@row\%\\@; c \\@col\%\\@; t
#' \\@tot\%\\@)''. See examples for more detail. Any call to \code{tableStat()} will
#' run \code{tableStat.default()}, with user specified values in place of the
#' appropriate defaults.
#' 
#' @aliases tableStat tableStat.default tableStat.do print.tableStat
#' @param variable \code{variable} a vector or \code{Surv}
#' object suitable for use as an argument to \code{descrip()}. If a \code{NULL}
#' value is supplied for \code{variable}, the valid statistics returned by the
#' function is only the cross-tabulation of counts and percentages within
#' strata.
#' @param \dots \code{\dots} an arbitrary number of
#' stratification variables. The arguments can be vectors, matrices, or lists.
#' Individual columns of a matrix or elements of a list may be of class
#' \code{numeric}, \code{factor}, or \code{character}. Stratification variables
#' must all be the same length as each other and (if it is supplied) variable.
#' @param stat a vector of character strings indicating the descriptive
#' statistic(s) to be tabulated within strata. Possibilities include any
#' statistic returned by descrip() as specified by one or more of ``count'',
#' ``missing'', ``mean'', ``geometric mean'', ``median'', ``sd'', ``variance'',
#' ``minimum'', ``maximum'', ``quantiles'', ``probabilities'', ``mn(sd)'',
#' ``range'', ``iqr'', ``all'', ``row\%'', ``col\%'', or ``tot\%''. Only enough
#' of the string needs to be specified to disambiguate the choice.
#' Alternatively (and more usefully), a single special format character string
#' can be specified as described in the Details below.
#' @param printer a logical indicating whether or not the function should
#' return the values necessary for a print with special characters as laid out
#' in \code{stat}.
#' @param na.rm an indicator that missing data is to be removed prior to
#' computation of the descriptive statistics.
#' @param subset \code{subset} vector indicating a subset
#' to be used for all descriptive statistics. If \code{subset} is supplied, all
#' variables must be of that same length.
#' @param probs \code{probs} a vector of probabilities
#' between 0 and 1 indicating quantile estimates to be included in the
#' descriptive statistics. Default is to compute 25th, 50th (median) and 75th
#' percentiles.
#' @param replaceZeroes \code{restriction} if not
#' \code{FALSE}, this indicates a value to be used in place of zeroes when
#' computing a geometric mean. If \code{TRUE}, a value equal to one-half the
#' lowest nonzero value is used. If a numeric value is supplied, that value is
#' used for all variables.
#' @param restriction \code{restriction} a value used for
#' computing restricted means, standard deviations, and geometric means with
#' censored time to event data. The default value of \code{Inf} will cause
#' restrictions at the highest observation. Note that the same value is used
#' for all variables of class \code{Surv}.
#' @param above \code{above} a vector of values used to
#' dichotomize variables. The descriptive statistics will include an estimate
#' for each variable of the proportion of measurements with values greater than
#' each element of \code{above}.
#' @param below \code{below} a vector of values used to
#' dichotomize variables. The descriptive statistics will include an estimate
#' for each variable of the proportion of measurements with values less than
#' each element of \code{below}.
#' @param labove \code{labove} a vector of values used to
#' dichotomize variables. The descriptive statistics will include an estimate
#' for each variable of the proportion of measurements with values greater than
#' or equal to each element of \code{labove}.
#' @param rbelow \code{rbelow} a vector of values used to
#' dichotomize variables. The descriptive statistics will include an estimate
#' for each variable of the proportion of measurements with values less than or
#' equal to each element of \code{rbelow}.
#' @param lbetween \code{lbetween} a vector of values with
#' \code{-Inf} and \code{Inf} appended is used as cutpoints to categorize
#' variables. The descriptive statistics will include an estimate for each
#' variable of the proportion of measurements with values between successive
#' elements of \code{lbetween}, with the left hand endpoint included in each
#' interval.
#' @param rbetween a vector of values with
#' \code{-Inf} and \code{Inf} appended is used as cutpoints to categorize
#' variables. The descriptive statistics will include an estimate for each
#' variable of the proportion of measurements with values between successive
#' elements of \code{rbetween}, with the right hand endpoint included in each
#' interval.
#' @param interval a two column matrix of
#' values in which each row is used to define intervals of interest to
#' categorize variables. The descriptive statistics will include an estimate
#' for each variable of the proportion of measurements with values between two
#' elements in a row, with neither endpoint included in each interval.
#' @param linterval a two column matrix
#' of values in which each row is used to define intervals of interest to
#' categorize variables. The descriptive statistics will include an estimate
#' for each variable of the proportion of measurements with values between two
#' elements in a row, with the left hand endpoint included in each interval.
#' @param rinterval a two column matrix
#' of values in which each row is used to define intervals of interest to
#' categorize variables. The descriptive statistics will include an estimate
#' for each variable of the proportion of measurements with values between two
#' elements in a row, with the right hand endpoint included in each interval.
#' @param lrinterval a two column matrix
#' of values in which each row is used to define intervals of interest to
#' categorize variables. The descriptive statistics will include an estimate
#' for each variable of the proportion of measurements with values between two
#' elements in a row, with both endpoints included in each interval.
#' @param version \code{version} If \code{TRUE}, the
#' version of the function will be returned. No other computations will be
#' performed.
#' @return An object of class \code{tableStat}
#' is returned, which consists of a list of arrays. Each array corresponds to a
#' table of stratified statistics for one of the possible choices of
#' \code{stat}. The print method provides the formatted output for the choice
#' specified in \code{stat}. 
#' @examples
#' 
#' # Load required libraries
#' library(survival)
#' 
#' # Reading in a dataset
#' mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt",header=TRUE)
#' 
#' # Creating a Surv object to reflect time to death
#' mri$ttodth <- Surv(mri$obstime,mri$death)
#' 
#' # Reformatting an integer MMDDYY representation of date to be a Date object
#' mri$mridate <- as.Date(paste(trunc(mri$mridate/10000),trunc((mri$mridate %% 10000)/100),
#' mri$mridate %% 100,sep="/"),"%m/%d/%y")
#' 
#' # Cross tabulation of counts with sex and race strata
#' with (mri, tableStat (NULL, race, male, stat= "@count@ (r @row%@; c @col%@; t @tot%@)"))
#' 
#' # Cross tabulation of counts with sex, race, and coronary disease strata
#' # (Note row and column percentages are defined within the first two strata, while overall
#' # percentage considers all strata)
#' with (mri, tableStat (NULL, race, male, chd,
#' stat= "@count@ (r @row%@; c @col%@; t @tot%@)"))
#' 
#' # Description of time to death with appropriate quantiles
#' with (mri, tableStat(ttodth,probs=c(0.05,0.1,0.15,0.2),
#' stat="mean @mean@ (q05: @q@; q10: @q@; q15: @q@; q20: @q@; max: @max@)"))
#' 
#' # Description of mridate with mean, range stratified by race and sex
#' with (mri, tableStat(mridate, race, male,
#' stat="mean @mean@ (range @min@ - @max@)"))
#' 
#' # Stratified descriptive statistics with proportions
#' with (mri, tableStat(age,stat=">75: @p@; >85: @p@; [-Inf,75): @p@; [75,85): @p@; 
#'       [85,Inf): @p@"), above=c(75,85),lbetween=c(75,85))
#' 
#' # Descriptive statistics on a subset comprised of males
#' with (mri, tableStat(dsst,age,stroke,subset=male==1,
#' stat="@mean@ (@sd@; n= @count@/@missing@)"))
#' 
#' 
#' @export tableStat
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
