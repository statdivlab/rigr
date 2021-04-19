#' One Sample Inferential Methods
#' 
#' Produces point estimates, interval estimates, and p values for an arbitrary
#' functional (mean, geometric mean, proportion, median, quantile, odds) of a
#' variable of class \code{integer}, \code{numeric}, \code{Surv}, or
#' \code{Date}. A variety of inferential methods are provided, with the choices
#' depending on the functional and the data type.
#' 
#' Default values for inference correspond to the most commonly implemented methods.
#' Additional methods are provided more for educational purposed than for
#' purposes of statistical analysis.
#' 
#' @aliases oneSample binomInference.exactLR binomInference.exactTail
#' binomInference.halfP binomInference.jeffreys binomInference.wald
#' binomInference.cwald binomInference.score binomInference.cscore
#' binomInference.agresti KMinference.ident CIptKM CIefrKM CIhwKM qSupBrnMotn
#' qSupBrnBrdg print.uOneSample
#' @param fnctl \code{fnctl} a character string indicating
#' the functional (summary measure of the distribution) for which inference is
#' desired. Choices include \code{"mean"}, \code{"geometric mean"},
#' \code{"proportion"}, \code{"median"}, \code{"quantile"}, \code{"odds"},
#' \code{"rate"}. The character string may be shortened to a unique substring.
#' Hence \code{"mea"} will suffice for \code{"mean"}.
#' @param y \code{y} a variable for which inference is
#' desired. The variable may be of class \code{numeric}, \code{Surv}, or
#' \code{Date}.
#' @param null.hypothesis \code{null.hypothesis} a numeric
#' scalar indicating any null hypothesis to be tested.
#' @param test.type \code{test.type} a character string
#' indicating whether a hypothesis test is to be of a one sided test of a
#' lesser alternative hypothesis (\code{"less"}), a one sided test of a greater
#' alternative hypothesis (\code{"greater"}), or a test of a two sided
#' alternative hypothesis (\code{"two.sided"}). The default value is
#' \code{"two.sided"}.
#' @param subset \code{subset} a vector indicating a
#' subset to be used for all inference.
#' @param conf.level \code{conf.level} a numeric scalar
#' indicating the level of confidence to be used in computing confidence
#' intervals. The default is 0.95.
#' @param na.rm \code{na.rm} an indicator that missing
#' data is to be removed prior to computation of the descriptive statistics.
#' @param probs \code{probs} a vector of probabilities
#' between 0 and 1 indicating quantile estimates to be included in the
#' descriptive statistics. Default is to the 50th (median) percentile.
#' @param replaceZeroes \code{replaceZeroes} if not
#' \code{FALSE}, this indicates a value to be used in place of zeroes when
#' computing a geometric mean. If \code{TRUE}, a value equal to one-half the
#' lowest nonzero value is used. If a numeric value is supplied, that value is
#' used for all variables.
#' @param restriction \code{restriction} a value used for
#' computing restricted means, standard deviations, and geometric means with
#' censored time to event data. The default value of \code{Inf} will cause
#' restrictions at the highest observation. Note that the same value is used
#' for all variables of class \code{Surv}.
#' @param subjTime \code{subjTime} a vector of values for
#' use with rates.
#' @param method \code{method} a character string used to
#' indicate inferential methods. Allowed choices depend on the variable type
#' and the functional. Default values are \code{"t.test"} for means and
#' geometric means, and \code{"exact"} for proportions of uncensored data, and
#' \code{"KM"} for censored survival data.
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
#' @param interval \code{interval} a two column matrix of
#' values in which each row is used to define intervals of interest to
#' categorize variables. The descriptive statistics will include an estimate
#' for each variable of the proportion of measurements with values between two
#' elements in a row, with neither endpoint included in each interval.
#' @param linterval \code{linterval} a two column matrix
#' of values in which each row is used to define intervals of interest to
#' categorize variables. The descriptive statistics will include an estimate
#' for each variable of the proportion of measurements with values between two
#' elements in a row, with the left hand endpoint included in each interval.
#' @param rinterval \code{rinterval} a two column matrix
#' of values in which each row is used to define intervals of interest to
#' categorize variables. The descriptive statistics will include an estimate
#' for each variable of the proportion of measurements with values between two
#' elements in a row, with the right hand endpoint included in each interval.
#' @param lrinterval \code{lrinterval} a two column matrix
#' of values in which each row is used to define intervals of interest to
#' categorize variables. The descriptive statistics will include an estimate
#' for each variable of the proportion of measurements with values between two
#' elements in a row, with both endpoints included in each interval.
#' @param g1 \code{g1} used in
#' \code{method="mean-variance"}.
#' @param g2 \code{g2} used in
#' \code{method="mean-variance"}.
#' @param dispersion \code{dispersion} dispersion, used in
#' \code{method="mean-variance"}.
#' @param nbstrap \code{nbstrap} number of bootstrap
#' iterations to perform, used with \code{method="bootstrap"}.
#' @param resample \code{resample} character string
#' specifying how the bootstrap should resample, used with
#' \code{method="bootstrap"}.
#' @param seed \code{seed} sets the seed (for random
#' number generation), used with \code{method="bootstrap"}.
#' @param \dots \code{\dots} other arguments.
#' @param version \code{version} if \code{TRUE}, the
#' version of the function will be returned. No other computations will be
#' performed.
#' @return An object of class \code{uOneSample}
#' is returned. Inferential statistics are contained in a vector named
#' \code{$Inference} that includes the sample size, the point estimate, the
#' lower and upper bounds of a confidence interval, any null hypothesis that
#' was specified, and the p-value. Also included is a vector named
#' \code{$Statistics} that includes more technical information. There is a
#' print method that will format the descriptive statistics for the \code{Date}
#' and \code{Surv} objects. 
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
#' # Inference about the mean LDL: a two sample t test that mean LDL is 135 mg/dl
#' oneSample ("mean", mri$ldl, null.hypothesis=125)
#' 
#' # Inference about the mean LDL: a one sample t test of a lesser alternative
#' # that mean LDL is 125 mg/dl
#' oneSample ("mean", mri$ldl, null.hypothesis=125, test.type="less")
#' 
#' # Inference about the mean LDL: a one sample t test of a greater alternative
#' # that mean LDL is 125 mg/dl
#' oneSample ("mean", mri$ldl, null.hypothesis=125, test.type="greater")
#' 
#' # Inference about the geometric mean LDL: a one sample t test of a greater
#' # alternative that geometric mean LDL is 125 mg/dl
#' oneSample ("geom", mri$ldl, null.hypothesis=125, test.type="greater")
#' 
#' # Inference about the proportion of subjects with LDL greater than 128: exact binomial
#' # inference that 50% of subjects have LDL greater than 128 mg/dl
#' oneSample ("prop", mri$ldl, null.hypothesis=0.5, above=128)
#' oneSample ("prop",mri$ldl>128, null.hypothesis=0.5)
#' 
#' 
#' @export oneSample
oneSample<-function (fnctl, y, null.hypothesis = NA, test.type = "two.sided", 
          subset = rep(TRUE, N), conf.level = 0.95, na.rm = TRUE, probs = 0.5, 
          replaceZeroes = NULL, restriction = Inf, subjTime = rep(1, 
          N), method = NULL, above = NULL, below = NULL, 
          labove = NULL, rbelow = NULL, interval = NULL, linterval = NULL, 
          rinterval = NULL, lrinterval = NULL, g1 = 1, g2 = 0, dispersion = 1, 
          nbstrap = 10000, resample = "pairs", seed = 42, ..., version = FALSE) 
{
  # Version check
  vrsn <- "20160913"
  if (version) 
    return(vrsn)
  boot.var<-function(x,probs,nbstrap,seed=seed){
    # set seed for reproducibility
    set.seed(seed = seed)
    # order samples 
    n<-length(x)
    smpl<-sample(x,size=n*nbstrap,replace=TRUE)
    indx<-matrix(c(rep(1:nbstrap,n)),nrow=n,byrow=T)
    bstrap<-matrix(smpl[order(indx,smpl)],ncol=nbstrap,byrow=F)
    # Calculate the quantile you want
    nindx<-probs*(n-1)+1
    is_int <- nindx%%1==0
    if (nindx%%1==0) {
      qntl<-t(bstrap[nindx,])
    } else {
      dif<-1/(n-1)
      nindxu<-ceiling(nindx)
      nindxl<-floor(nindx)
      pnindxu<-(nindxu-1)/(n-1)
      pnindxl<-(nindxl-1)/(n-1)
      qntl<-c((pnindxu-probs)/dif,(probs-pnindxl)/dif)%*%bstrap[nindxl:nindxu,]
    }
    return(sd(t(qntl)))
  }
  km_val <- function(km,time,val.indx=8,left.shift=FALSE, default=0){
    time.index=2
    if (left.shift){
      if(time <= km[[time.index]][1]) return(default)
      indx <-pmatch(max(km[[time.index]][km[[time.index]]<time]),km[[time.index]])
    }
    else {
      if(time < km[[time.index]][1]) return(default)
      indx <-pmatch(max(km[[time.index]][km[[time.index]]<=time]),km[[time.index]])
    }
    return(km[[val.indx]][indx])
  }
  
  # Check which functional is requested
  chc.fnctl <- c("mean", "geometric mean", "proportion", "median", 
                 "quantile", "odds")
  name.fnctl <- c("Mean", "GeomMn", "Prop", "Mdn", paste(format(100 * 
                                                                     probs[1]), "%ile", sep = ""), "Odds", "Rate")
  findx <- pmatch(fnctl, chc.fnctl)
  if (is.na(findx)) 
    stop("unsupported functional")
  
  # check data input to see if it's valid
  fnctl <- chc.fnctl[findx]
  isSurv <- is.Surv(y)
  isDate <- inherits(y, "Date")
  isEvent <- is.logical(y) || all(y[!is.na(y)] %in% 0:1)
  if (!isSurv && !isDate && !isEvent && !is.numeric(y)) 
    stop("y is an unsupported data type")
  
  # Prep ya, which is y after restrictions and NA's removed (if specified)
  if (isSurv) {
    N <- dim(y)[1]
    if (restriction < Inf) {
        u <- y[, 1] > restriction
        y[u, 1] <- restriction
        y[u, 2] <- 0
        rName <- paste("Summary measure restricted to", format(restriction))
      }
    else rName <- NULL
    if (na.rm){
      u <- !is.na(y[,1])
      nMsng <- sum(subset & !u)
      }
    else {
      u <- rep(TRUE, N)
      nMsng <- NA
    }
    ya <- y[subset & u]
    n <- dim(ya)[1]
    default.method <- "KM"
  }
  else {
    N <- length(y)
    rName <- NULL
    y[y > restriction] <- restriction
    if (na.rm){
      u <- !is.na(y)
      nMsng <- sum(subset & !u)
    }
    else {
      u <- rep(TRUE, N)
      nMsng <- NA
    }
    ya <- y[subset & u]
    n <- length(ya)
    default.method <- c("t.test", "t.test", "exact", "bootstrap", 
                        "bootstrap", "exact", "mean-variance")[findx]
  }
  
  # check the requested method
  chc.method <- c("t.test", "exact", "exactLR", "exactTail", 
                  "wald", "cwald", "score", "cscore", "agresti", "jeffreys", 
                  "sign", "bootstrap", "mean-variance", "KM")
  if (is.null(method)) 
    method <- default.method
  mindx <- pmatch(method, chc.method)
  if (is.na(mindx)) 
    stop("unsupported method")
  method <- chc.method[mindx]
  
  # If intervals are specified, create them
  thresholds <- NULL
  
  if (length(above) > 0) 
    thresholds <- rbind(thresholds, cbind(0, above, 0, Inf))
  if (length(below) > 0) 
    thresholds <- rbind(thresholds, cbind(0, -Inf, 0, below))
  if (length(labove) > 0) 
    thresholds <- rbind(thresholds, cbind(1, labove, 0, Inf))
  if (length(rbelow) > 0) 
    thresholds <- rbind(thresholds, cbind(0, -Inf, 1, rbelow))
  if (!is.null(interval)) {
    if (length(interval) == 2) 
      interval <- matrix(interval, ncol = 2)
    if (dim(interval)[2] != 2) 
      stop("intervals must be specified in a 2 column matrix")
    thresholds <- rbind(thresholds, cbind(0, interval[, 1], 
                                          0, interval[, 2]))
  }
  if (!is.null(linterval)) {
    if (length(linterval) == 2) 
      linterval <- matrix(linterval, ncol = 2)
    if (dim(linterval)[2] != 2) 
      stop("intervals must be specified in a 2 column matrix")
    thresholds <- rbind(thresholds, cbind(1, linterval[, 
                                                       1], 0, linterval[, 2]))
  }
  if (!is.null(rinterval)) {
    if (length(rinterval) == 2) 
      rinterval <- matrix(rinterval, ncol = 2)
    if (dim(rinterval)[2] != 2) 
      stop("intervals must be specified in a 2 column matrix")
    thresholds <- rbind(thresholds, cbind(0, rinterval[, 
                                                       1], 1, rinterval[, 2]))
  }
  if (!is.null(lrinterval)) {
    if (length(lrinterval) == 2) 
      lrinterval <- matrix(lrinterval, ncol = 2)
    if (dim(lrinterval)[2] != 2) 
      stop("intervals must be specified in a 2 column matrix")
    thresholds <- rbind(thresholds, cbind(1, lrinterval[, 
                                                        1], 1, lrinterval[, 2]))
  }
  if (is.null(thresholds)) 
    thresholds <- rbind(thresholds, cbind(0, 0, 0, Inf))
  thresholds <- thresholds[1, , drop = F]
  
  if (fnctl %in% c("proportion", "odds")) {
    if (isEvent) 
      fName <- ifelse(fnctl == "proportion", "Pr(Event)", 
                      "Odds(Event)")
    else fName <- paste(sep = "", ifelse(fnctl == "proportion", 
                                         "Pr", "Odds"), ifelse(thresholds[, 2] == -Inf, paste(sep = "", 
                                                                                              ifelse(thresholds[, 3] == 0, "<", "<="), format(thresholds[, 
                                                                                                                                                         4])), ifelse(thresholds[, 4] == Inf, paste(sep = "", 
                                                                                                                                                                                                    ifelse(thresholds[, 1] == 0, ">", ">="), format(thresholds[, 
                                                                                                                                                                                                                                                               2])), paste(sep = "", ifelse(thresholds[, 1] == 
                                                                                                                                                                                                                                                                                              0, "(", "["), format(thresholds[, 2]), ",", format(thresholds[, 
                                                                                                                                                                                                                                                                                                                                                            4]), ifelse(thresholds[, 3] == 0, ")", "]")))))
    if (!isSurv) 
      ya <- ifelse1(thresholds[, 1] == 0, ya > thresholds[, 
                                                          2], ya >= thresholds[, 2]) & ifelse1(thresholds[, 
                                                                                                          3] == 0, ya < thresholds[, 4], ya <= thresholds[, 
                                                                                                                                                          4])
  }
  else fName <- name.fnctl[findx]
  
  # Figure out which test is specified
  chc.test <- c("greater", "less", "two.sided")
  tindx <- pmatch(test.type, chc.test)
  if (is.na(tindx)) 
    stop("unsupported test type")
  test.type <- chc.test[tindx]
  if (is.na(null.hypothesis)) {
    hName <- NULL
    tindx <- 0
  }
  else hName <- paste("Hypothesis test of", c("upper", "lower", 
                                              "two-sided")[tindx], "alternative that", fName, c(">", 
                                                                                                "<", "<>")[tindx], format(null.hypothesis))
  chc.resample <- c("pairs", "independent")
  if (!is.null(resample)) {
    rindx <- pmatch(resample, chc.method)
    if (is.na(mindx)) 
      stop("unsupported method")
    resample <- chc.resample[rindx]
  }
  nReplace <- NA
  if (isSurv){
    if (fnctl == "mean") {
      if (method=="KM") {
        fit_sum<-summary(survfit(ya~1))
        mName <- "KM"
        link <- "identity"
        etaName <- fName
        etaHat <- fit_sum$table[["*rmean"]]
        etaHatSE <- fit_sum$table[["*se(rmean)"]]
        etaNull <- null.hypothesis
        statistic <- (etaHat - etaNull)/etaHatSE
        df <- NA
        if (test.type == "less") 
          p.value <- pnorm(statistic)
        else if (test.type == "greater") 
          p.value <- 1 - pnorm(statistic)
        else if (test.type == "two.sided") 
          p.value <- 2 * pnorm(-abs(statistic))
        else p.value <- NA
        etaCIlo <- qnorm((1 - conf.level)/2) * etaHatSE
        etaCIhi <- etaHat - etaCIlo
        etaCIlo <- etaHat + etaCIlo
        thetaHat <- etaHat
        thetaCIlo <- etaCIlo
        thetaCIhi <- etaCIhi
        methodParams <- NULL
        }
      else stop(paste("method", method, "not yet implemented"))
    }
    else if (fnctl == "geometric mean") {
      if (!is.null(replaceZeroes)) {
        u <- ya[,1] == 0
        nReplace <- sum(u)
        if (is.logical(replaceZeroes)) 
          replaceZeroes <- min(ya[!u,1])
        ya[u,1] <- replaceZeroes
      }
      else if (any(ya[,1] <= 0)) stop("cannot compute geometric mean with nonpositive data")
      if (method=="KM") {
        mName <- "KM on log transformed data"
        ya[,1]<-log(ya[,1])
        fit_sum<-summary(survfit(ya~1))
        mName <- "KM"
        link <- "log"
        etaName <- fName
        etaHat <- fit_sum$table[["*rmean"]]
        etaHatSE <- fit_sum$table[["*se(rmean)"]]
        etaNull <- log(null.hypothesis)
        statistic <- (etaHat - etaNull)/etaHatSE
        df <- NA
        if (test.type == "less") 
          p.value <- pnorm(statistic, df)
        else if (test.type == "greater") 
          p.value <- 1 - pnorm(statistic, df)
        else if (test.type == "two.sided") 
          p.value <- 2 * pnorm(-abs(statistic), df)
        else p.value <- NA
        etaCIlo <- qnorm((1 - conf.level)/2, df) * etaHatSE
        etaCIhi <- etaHat - etaCIlo
        etaCIlo <- etaHat + etaCIlo
        thetaHat <- etaHat
        thetaCIlo <- etaCIlo
        thetaCIhi <- etaCIhi
        methodParams <- NULL
      }
      else stop(paste("method", method, "not yet implemented"))
    }
    else if (fnctl == "proportion" || fnctl == "odds") {
      if (method == "KM") {
        mName <- "KM"
        etaName <- fName
        intrvl <- rep(0,2)
        Sya <- survfit(ya~1)
        if (length(above) >0 ) {
          if(length(above)==1) {
            intrvl[1] <- km_val(Sya, above, val.indx = 2, left.shift = FALSE, default = 1)
            intrvl[2] <- km_val(Sya, Inf, val.indx = 2, left.shift = FALSE, default = 1)
          }
          else if (length(above) > 1)
            stop("support for only one interval at a time is supported")
        }
        if (length(below) > 0) {
          if(length(below)==1) {
            intrvl[2] <- km_val(Sya, below, val.indx = 2, left.shift = TRUE, default = 1)
          }
          else if (length(below) > 1)
            stop("support for only one interval at a time is supported")
        }
        if (length(labove) > 0) {
          if(length(labove)==1){
            intrvl[1] <- km_val(Sya, labove, val.indx = 2, left.shift = TRUE, default = 1)
            intrvl[2] <- km_val(Sya, Inf, val.indx = 2, left.shift = FALSE, default = 1)
          }
          else if (length(labove) > 1)
            stop("support for only one interval at a time is supported")
        }
        if (length(rbelow) > 0) {
          if(length(rbelow)==1){
            intrvl[2] <- km_val(Sya, rbelow, val.indx = 2, left.shift = FALSE, default = 1)
          }
          else if (length(rbelow) > 1)
            stop("support for only one interval at a time is supported")
        }
        if (!is.null(interval)) {
          if (length(interval) == 2) {
            interval <- sort(interval)
            intrvl[2] <- km_val(Sya, interval[2], val.indx = 2, left.shift = TRUE, default = 1)
          }
        }
        if (!is.null(linterval)) {
          if (length(linterval) == 2) {
            linterval <- sort(linterval)
            intrvl[1]<-km_val(Sya, linterval[1], val.indx = 2, left.shift = TRUE, default = 1)
            intrvl[2]<-km_val(Sya, linterval[2], val.indx = 2, left.shift = TRUE, default = 1)
          }
        }
        if (!is.null(rinterval)) {
          if (length(rinterval) == 2) 
            intrvl <- sort(rinterval)
        }
        if (!is.null(lrinterval)) {
          if (length(lrinterval) == 2) {
            lrinterval <- sort(lrinterval)
            intrvl[1] <- km_val(Sya, lrinterval[1], val.indx = 2, left.shift = TRUE, default = 1)
            intrvl[2] <- lrinterval[2]
          }
        }
        
        etaHat <- abs(diff(summary(Sya,times=intrvl)[[6]]))
        etaHatSE <- km_val(Sya, intrvl[1], val.indx = 8, default = 0) + 
                      km_val(Sya, intrvl[2], val.indx = 8, default = 0)
        etaNull <- null.hypothesis
        statistic <- (etaHat - etaNull)/etaHatSE
        df <- NA
        if (test.type == "less") 
          p.value <- pnorm(statistic)
        else if (test.type == "greater") 
          p.value <- 1 - pnorm(statistic)
        else if (test.type == "two.sided") 
          p.value <- 2 * pnorm(-abs(statistic))
        else p.value <- NA
        etaCIlo <- qnorm((1 - conf.level)/2) * etaHatSE
        etaCIhi <- min(1,etaHat - etaCIlo)
        etaCIlo <- max(0,etaHat + etaCIlo)
        thetaHat <- etaHat
        thetaCIlo <- etaCIlo
        thetaCIhi <- etaCIhi
        methodParams <- NULL
        
        if (fnctl == "proportion") {
          link <- "identity"
          mName <- paste("One sample inference for binomial proportions using", 
                         mName)
          thetaHat <- etaHat
          thetaCIlo <- etaCIlo
          thetaCIhi <- etaCIhi
        }
        else {
          link <- "logit"
          mName <- paste("One sample inference for binomial odds using", 
                         mName)
          thetaHat <- etaHat/(1 - etaHat)
          thetaCIlo <- etaCIlo/(1 - etaCIlo)
          thetaCIhi <- etaCIhi/(1 - etaCIhi)
        }
      }
      else stop(paste("method", method, "not yet implemented"))
    }
    else if (fnctl == "median" || fnctl == "quantile") {
      if (fnctl == "median") probs=0.5
      if (method =="KM") {
      quant<-quantile(survfit(ya~1,conf.int=conf.level),probs=probs)
      mName <- "KM"
      link <- NA
      etaName <- fName
      etaHat <- quant$quantile
      etaCIlo <- quant$lower
      etaCIhi <- quant$upper
      etaNull<-null.hypothesis
      statistic<-NA
      df<-NA
      p.value<-NA
      etaHatSE<-NA
      thetaHat <- etaHat
      thetaCIlo <- etaCIlo
      thetaCIhi <- etaCIhi
      methodParams <- NULL
      }
      else stop(paste("method", method, "not yet implemented"))
    }
    else stop(paste("inference for", fnctl, "not yet implemented"))
  }
  else {
    if (fnctl == "mean") {
      if (method == "t.test") {
        mName <- "One sample t test"
        link <- "identity"
        etaName <- fName
        etaHat <- mean(ya)
        etaHatSE <- sqrt(var(ya)/n)
        etaNull <- null.hypothesis
        statistic <- (etaHat - etaNull)/etaHatSE
        df <- n - 1
        if (test.type == "less") 
          p.value <- pt(statistic, df)
        else if (test.type == "greater") 
          p.value <- 1 - pt(statistic, df)
        else if (test.type == "two.sided") 
          p.value <- 2 * pt(-abs(statistic), df)
        else p.value <- NA
        etaCIlo <- qt((1 - conf.level)/2, df) * etaHatSE
        etaCIhi <- etaHat - etaCIlo
        etaCIlo <- etaHat + etaCIlo
        thetaHat <- etaHat
        thetaCIlo <- etaCIlo
        thetaCIhi <- etaCIhi
        methodParams <- NULL
      }
    }
    else if (fnctl == "geometric mean") {
      if (!is.null(replaceZeroes)) {
        u <- ya == 0
        nReplace <- sum(u)
        if (is.logical(replaceZeroes)) 
          replaceZeroes <- min(ya[!u])
        ya[u] <- replaceZeroes
      }
      else if (any(ya <= 0)) 
        stop("cannot compute geometric mean with nonpositive data")
      if (method == "t.test") {
        mName <- "One sample t test on log transformed data"
        link <- "log"
        etaName <- fName
        etaHat <- mean(log(ya))
        etaHatSE <- sqrt(var(log(ya))/n)
        etaNull <- log(null.hypothesis)
        statistic <- (etaHat - etaNull)/etaHatSE
        df <- n - 1
        if (test.type == "less") 
          p.value <- pt(statistic, df)
        else if (test.type == "greater") 
          p.value <- 1 - pt(statistic, df)
        else if (test.type == "two.sided") 
          p.value <- 2 * pt(-abs(statistic), df)
        else p.value <- NA
        etaCIlo <- qt((1 - conf.level)/2, df) * etaHatSE
        etaCIhi <- etaHat - etaCIlo
        etaCIlo <- etaHat + etaCIlo
        thetaHat <- exp(etaHat)
        thetaCIlo <- exp(etaCIlo)
        thetaCIhi <- exp(etaCIhi)
        methodParams <- NULL
      }
      else stop(paste("method", method, "not yet implemented"))
    }
    else if (fnctl == "proportion" || fnctl == "odds") {
      if (method == "KM") 
        stop("KM method not applicable for non-survival objects")
      else {
        if (method == "exact" || method == "exactLR") {
          binomInference <- binomInference.exactLR
          mName <- "exact distribution"
          if (test.type == "two.sided") 
            mName <- paste(mName, "(LR ordering)")
        }
        else if (method == "exactTail") {
          binomInference <- binomInference.exactTail
          mName <- "exact distribution"
          if (test.type == "two.sided") 
            mName <- paste(mName, "(tail probability ordering)")
        }
        else if (method == "wald") {
          binomInference <- binomInference.wald
          mName <- "Wald statistic"
        }
        else if (method == "cwald") {
          binomInference <- binomInference.cwald
          mName <- "continuity corrected Wald statistic"
        }
        else if (method == "score") {
          binomInference <- binomInference.score
          mName <- "score statistic"
        }
        else if (method == "cscore") {
          binomInference <- binomInference.cscore
          mName <- "continuity corrected score statistic"
        }
        else if (method == "agresti") {
          binomInference <- binomInference.agresti
          mName <- "Agresti & Coull"
        }
        else if (method == "jeffreys") {
          binomInference <- binomInference.jeffreys
          mName <- "Jeffreys"
        }
        else stop(paste("method", method, "not yet implemented"))
        etaName <- fName
        z <- binomInference(y = sum(ya), n = length(ya), 
                            null.hypothesis = null.hypothesis, test.type = test.type, 
                            conf.level = conf.level)
        etaHat <- z[2]
        etaHatSE <- z[3]
        etaNull <- null.hypothesis
        statistic <- z[4]
        df <- NA
        p.value <- z[7]
        etaCIlo <- z[5]
        etaCIhi <- z[6]
        methodParams <- NULL
        if (fnctl == "proportion") {
          link <- "identity"
          mName <- paste("One sample inference for binomial proportions using", 
                         mName)
          thetaHat <- etaHat
          thetaCIlo <- etaCIlo
          thetaCIhi <- etaCIhi
        }
        else {
          link <- "logit"
          mName <- paste("One sample inference for binomial odds using", 
                         mName)
          thetaHat <- etaHat/(1 - etaHat)
          thetaCIlo <- etaCIlo/(1 - etaCIlo)
          thetaCIhi <- etaCIhi/(1 - etaCIhi)
        }
      }
    }
    else if (fnctl == "median" || fnctl == "quantile") {
      if (fnctl == "median") probs=0.5
      if (method == "bootstrap"){ 
        mName <- "bootstrap"
        link <- NA
        etaName <- fName
        etaHat <- quantile(ya,probs=probs)
        etaHatSE <- boot.var(ya,probs=probs,nbstrap=nbstrap,seed=seed)
        etaNull <- null.hypothesis
        statistic <- (etaHat - etaNull)/etaHatSE
        if (test.type == "less") 
          p.value <- pnorm(statistic)
        else if (test.type == "greater") 
          p.value <- 1 - pnorm(statistic)
        else if (test.type == "two.sided") 
          p.value <- 2 * pnorm(-abs(statistic))
        else p.value <- NA
        etaCIlo <- qnorm((1 - conf.level)/2) * etaHatSE
        etaCIhi <- etaHat - etaCIlo
        etaCIlo <- etaHat + etaCIlo
        thetaHat <- etaHat
        thetaCIlo <- etaCIlo
        thetaCIhi <- etaCIhi
        methodParams <- NULL
      }
      else stop(paste("method", method, "not yet implemented"))
    }
    else stop(paste("inference for", fnctl, "not yet implemented"))
  }
  Inference <- cbind(n, thetaHat, thetaCIlo, thetaCIhi, null.hypothesis, 
                     p.value)
  #Paste values for output
  dimnames(Inference) <- list("", c("n", fName, paste(format(100 * 
                                                               conf.level), "% CIlo", sep = ""), paste(format(100 * 
                                                                                                                conf.level), "% CIhi", sep = ""), "Null Hyp", c("P", 
                                                                                                                                                                "P hi", "P lo", "P two")[tindx + 1]))
  attr(Inference, "fnctl") <- fnctl
  attr(Inference, "hName") <- hName
  attr(Inference, "fName") <- fName
  attr(Inference, "method") <- method
  attr(Inference, "mName") <- mName
  attr(Inference, "methodParams") <- methodParams
  attr(Inference, "link") <- link
  attr(Inference, "isSurv") <- isSurv
  attr(Inference, "isDate") <- isDate
  attr(Inference, "isEvent") <- isEvent
  attr(Inference, "restriction") <- restriction
  attr(Inference, "rName") <- rName
  attr(Inference, "replaceZeroes") <- replaceZeroes
  attr(Inference, "nReplace") <- nReplace
  attr(Inference, "thresholds") <- thresholds
  attr(Inference, "nMsng") <- nMsng
  attr(Inference, "test.type") <- test.type
  attr(Inference, "conf.level") <- conf.level
  Statistics <- cbind(n, etaHat, etaHatSE, etaNull, statistic, 
                      df)
  dimnames(Statistics) <- list("", c("n", etaName, "SE", "Null", 
                                     "TestStat", "df"))
  attr(Statistics, "link") <- link
  rslt <- list(Inference = Inference, Statistics = Statistics)
  class(rslt) <- "uOneSample"
  rslt
}
