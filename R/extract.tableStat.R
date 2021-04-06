extract.tableStat <-
function (x, stat=attr(x,"stat"), na.rm=attr(x,"na.rm"), version=F) {
  
  vrsn <- "20121024"
  if (version) return(vrsn)
  
  # This function is used by clusterStats(). It presumes a "tableStat" object created with
  #     - stratified statistics on a single variable that had no missing data (this is 
  #       satisfied if any missing strata values formed their own strata),
  #     - only a single quantile or a single probability will have been generated
  #       (actually it returns the first quantile or first probability)
  #
  # Only the first element of stat is used, and it cannot be a special format
  #
  # It returns
  #     - numeric vectors for all probabilities
  #     - for uncensored numeric data, it returns a numeric vector for all statistics
  #     - for censored data, the minimum, maximum, and quantiles are returned as "Surv" objects
  #     - for censored data, means, geometric means, sd, variances are returned with restrictions
  #       as an attribute
  #     - dates, it returns dates for all means, minimum, maximum, quantiles and numeric vectors
  #       for sd and variances
  
  #if (!inherits(x,"tableStat")) stop("x must be a tableStat object")
  if(class(x) != "tableStat") stop("x must be a tableStat object")
  ns <- dim(x[[1]])
  if (length(ns) > 1) stop("only a single stratification variable can be used")
  isDate <- attr(x,"isDate")
  orgn <- "1970-01-01"
  stat.chc <- c("count", "missing", "mean", "geometric mean", "median", "sd", "variance", "minimum", "maximum", "quantiles",
                "probabilities")
  stat <- stat[1]
  indx <- pmatch(stat,stat.chc)
  if (is.na(indx)) stop("statistic not recognized")
  xindx <- list(1,2,3,5,(1:length(x))[names(x)==" Mdn"],4,4,6,(1:length(x))[names(x)==" Max"],
                7:((1:length(x))[names(x)==" Max"]-1),((1:length(x))[names(x)==" Max"]+1):(length(x)-4))
  if (length(xindx[[10]]) == 2 && xindx[[10]][1] > xindx[[10]][2]) xindx[[10]] <- NA
  if (length(xindx[[11]]) == 2 && xindx[[11]][1] > xindx[[11]][2]) xindx[[11]] <- NA
  if (length(xindx[[5]])==0) xindx[[5]] <- NA
  specialLabel <- list("Cnt","Msng","Mean","GeomMn","Mdn","SD",
                       "Vrnc","Min","Max",paste(names(x)[xindx[[10]]],"ile",sep=""),names(x)[xindx[[11]]])
  
  if (!na.rm) {
    for (i in 3:length(x)) x[[i]][x[[2]]>0 & !is.nan(x[[2]])] <- NA
  } else {
    x[[1]] <- x[[1]] - x[[2]]
  }
  
  censMin <- x[["firstEvent"]] > x[[6]]
  censMin[is.na(censMin)] <- F
  censMax <- x[["lastEvent"]] < x[[xindx[[9]]]]
  censMax[is.na(censMax)] <- F
  
  if (any(is.na(xindx[[indx]][1]))) stop(paste("descriptives needed for",stat,"are unavailable"))
  rslt <- x[[xindx[[indx]][1]]][-ns]
  if (indx==7) rslt <- rslt^2
  if (indx==8 && any(censMin)) rslt <- Surv(rslt,1-censMin[-ns])
  else if (indx==9 && any(censMax)) rslt <- Surv(rslt,1-censMax[-ns])
  else if (indx %in% c(3,4,6,7) && any(x[["restriction"]]!=Inf)) {
    rslt <- Surv(rslt,1-censMax[-ns])
    attr(rslt,"restriction") <- x[[length(x)-3]][-ns]
  }
  if (isDate && indx %in% c(3:5,8:10)) rslt <- as.Date(rslt,orgn)
  attr(rslt,"statistic") <- specialLabel[[indx]][1]
  rslt
}
