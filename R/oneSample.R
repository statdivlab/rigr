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