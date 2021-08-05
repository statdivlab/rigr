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


binomInference.agresti <-
  function(y, n=length(y), null.hypothesis=NA, test.type="two.sided", conf.level=0.95, version=F) {
    
    vrsn <- "20141002"
    if (version) return(vrsn)
    if(length(y) > 1){
      if(length(y) != n){
        stop("n must be equal to the length of y")
      }
    }
    y <- sum(y)
    if(y < 0 | y > n) stop("y must be an integer between 0 and n inclusive")
    alpha <- (1 - conf.level) / 2
    y <- y + qnorm(alpha)^2 / 2
    n <- n + qnorm(alpha)^2
    phat <- y / n
    SE <- sqrt(phat * (1 - phat) / n)
    wlo <- phat + qnorm(alpha)*sqrt(phat * (1-phat) / n)
    whi <- phat - qnorm(alpha)*sqrt(phat * (1-phat) / n)
    if(!is.na(null.hypothesis)) {
      Z <- (phat - null.hypothesis) / SE
      plo <- pnorm(Z)
      phi <- 1 - pnorm(Z)
      p2 <- 2 * min(plo,phi)
      if(test.type=="less") p.value <- plo
      else if(test.type=="greater") p.value <- phi
      else p.value <- p2
    } else Z <- p.value <- NA
    rslt <- c(n, phat, SE, Z, wlo, whi, p.value)
    names(rslt) <- c("n", "Est", "SE", "Statistic", "CIlo", "CIhi", "p.value")
    attr(rslt,"method") = "Agresti"
    rslt
  }

binomInference.cscore <-
  function(y, n=length(y), null.hypothesis=NA, test.type="two.sided", conf.level=0.95, version=F) {
    
    vrsn <- "20141002"
    if (version) return(vrsn)
    if(length(y) > 1){
      if(length(y) != n){
        stop("n must be equal to the length of y")
      }
    }
    y <- sum(y)
    if(y < 0 | y > n) stop("y must be an integer between 0 and n inclusive")
    alpha <- (1 - conf.level) / 2
    phat <- y / n
    phatL <- phat - 1/2/n
    phatU <- phat + 1/2/n
    if(y==0) slo <- 0
    else slo <- (2 * n * phatL + qnorm(alpha)^2 - 
                   sqrt(4 * n * phatL * (1 - phatL) * qnorm(alpha)^2 + 
                          qnorm(alpha)^4)) / (2 * (n + qnorm(alpha)^2))
    if(y==n) shi <- 1
    else shi <- (2 * n * phatU + qnorm(alpha)^2 +
                   sqrt(4 * n * phatU * (1 - phatU) * qnorm(alpha)^2 +
                          qnorm(alpha)^4)) / (2 * (n + qnorm(alpha)^2))
    if(!is.na(null.hypothesis)) {
      SE <- sqrt(null.hypothesis * (1 - null.hypothesis) / n)
      Z <- (phat - null.hypothesis) / SE
      plo <- pnorm((phat + 1 / (2 * n) - null.hypothesis) / SE)
      phi <- 1 - pnorm((phat - 1 / (2 * n)  - null.hypothesis) / SE)
      p2 <- 2 * min(plo,phi)
      if(test.type=="less") p.value <- plo
      else if(test.type=="greater") p.value <- phi
      else p.value <- p2
    } else SE <- p.value <- NA
    rslt <- c(n, phat, SE, NA, slo, shi, p.value)
    names(rslt) <- c("n", "Est", "SE", "Statistic", "CIlo", "CIhi", "p.value")
    rslt
    attr(rslt,"method") <- "cScore"
    rslt
  }

binomInference.cwald <-
  function(y, n=length(y), null.hypothesis=NA, test.type="two.sided", conf.level=0.95, version=F) {
    
    vrsn <- "20141002"
    if (version) return(vrsn)
    if(length(y) > 1){
      if(length(y) != n){
        stop("n must be equal to the length of y")
      }
    }
    y <- sum(y)
    if(y < 0 | y > n) stop("y must be an integer between 0 and n inclusive")
    alpha <- (1 - conf.level) / 2
    phat <- y / n
    wlo <- phat - 1 / (2 * n) + qnorm(alpha)*sqrt(phat * (1-phat) / n)
    whi <- phat + 1 / (2 * n) - qnorm(alpha)*sqrt(phat * (1-phat) / n)
    if(!is.na(null.hypothesis)) {
      SE <- sqrt(phat * (1 - phat) / n)
      plo <- pnorm((phat + 1 / (2 * n) - null.hypothesis) / SE)
      phi <- 1 - pnorm((phat - 1 / (2 * n) - null.hypothesis) / SE)
      p2 <- 2 * min(plo,phi)
      if(test.type=="less") p.value <- plo
      else if(test.type=="greater") p.value <- phi
      else p.value <- p2
    } else SE <- p.value <- NA
    rslt <- c(n, phat, SE, NA, wlo, whi, p.value)
    names(rslt) <- c("n", "Est", "SE", "Statistic", "CIlo", "CIhi", "p.value")
    attr(rslt,"method") = "cWald"
    rslt
  }

binomInference.exactLR <-
  function(y, n=length(y), null.hypothesis=NA, test.type="two.sided", conf.level=0.95, version=F) {
    
    vrsn <- "20141002"
    if (version) return(vrsn)
    if(length(y) > 1){
      if(length(y) != n){
        stop("n must be equal to the length of y")
      }
    }
    y <- sum(y)
    if(y < 0 | y > n) stop("y must be an integer between 0 and n inclusive")
    alpha <- (1 - conf.level) / 2
    if(y==0) elo <-0
    else {
      elo <- qbeta(alpha, y, n-y+1)
    }
    if(y==n) ehi <- 1
    else {
      ehi <- qbeta(1-alpha,y+1,n-y)
    }
    if(!is.na(null.hypothesis)) {
      plo <- pbinom(y, n, null.hypothesis)
      phi <- 1 - plo + dbinom(y, n, null.hypothesis)
      pdf <- dbinom(0:n, n, null.hypothesis)
      if (plo < phi) {
        u <- (0:n) >= (n * null.hypothesis)
        p2 <- plo
      } else {
        u <- (0:n) <= (n * null.hypothesis)
        p2 <- phi
      }
      u <- u & (pdf <= dbinom(y, n, null.hypothesis) * (1 + 1e-7))
      if(sum(u) > 0) p2 <- min(p2 + sum(pdf[u]),1)
      if(test.type=="less") p.value <- plo
      else if(test.type=="greater") p.value <- phi
      else p.value <- p2
    } else p.value <- NA
    rslt <- c(n, y/n, NA, NA, elo, ehi, p.value)
    names(rslt) <- c("n", "Est", "SE", "Statistic", "CIlo", "CIhi", "p.value")
    attr(rslt,"method") = "Exact(LR)"
    rslt
  }

binomInference.exactTail <-
  function(y, n=length(y), null.hypothesis=NA, test.type="two.sided", conf.level=0.95, version=F) {
    
    vrsn <- "20141002"
    if (version) return(vrsn)
    if(length(y) > 1){
      if(length(y) != n){
        stop("n must be equal to the length of y")
      }
    }
    y <- sum(y)
    if(y < 0 | y > n) stop("y must be an integer between 0 and n inclusive")
    alpha <- (1 - conf.level) / 2
    if(y==0) elo <-0
    else {
      elo <- qbeta(alpha, y, n-y+1)
    }
    if(y==n) ehi <- 1
    else {
      ehi <- qbeta(1-alpha,y+1,n-y)
    }
    if(!is.na(null.hypothesis)) {
      plo <- pbinom(y, n, null.hypothesis)
      phi <- 1 - plo + dbinom(y, n, null.hypothesis)
      cdf <- pbinom(0:n, n, null.hypothesis)
      if (plo < phi) {
        cdf <- 1 - cdf
        p2 <- plo
      } else p2 <- phi
      u <- cdf <= p2 * (1 + 1e-7)
      if(sum(u) > 0) p2 <- min(p2 + max(cdf[u]),1)
      if(test.type=="less") p.value <- plo
      else if(test.type=="greater") p.value <- phi
      else p.value <- p2
    } else p.value <- NA
    rslt <- c(n, y/n, NA, NA, elo, ehi, p.value)
    names(rslt) <- c("n", "Est", "SE", "Statistic", "CIlo", "CIhi", "p.value")
    attr(rslt,"method") = "Exact(tail)"
    rslt
  }

binomInference.halfP <-
  function(y, n=length(y), null.hypothesis=NA, test.type="two.sided", conf.level=0.95, version=F) {
    
    vrsn <- "20141002"
    if (version) return(vrsn)
    if(length(y) > 1){
      if(length(y) != n){
        stop("n must be equal to the length of y")
      }
    }
    y <- sum(y)
    if(y < 0 | y > n) stop("y must be an integer between 0 and n inclusive")
    alpha <- (1 - conf.level) / 2
    if(y==0) hlo <- 0
    else {
      f <- function(x, n, y, alpha) pbinom(n-y, n, 1-x) - dbinom(n-y, n, 1-x) / 2 - alpha
      hlo <- uniroot(f, c(0,1), n=n, y=y, alpha=alpha, tol=1e-10)$root
    }
    if(y==n) hhi <- 1
    else {
      f <- function (x, n, y, alpha) pbinom(y, n, x) - dbinom(y, n, x) / 2- alpha
      hhi <- uniroot (f, c(0,1), n=n, y=y, alpha=alpha, tol=1e-10)$root
    }
    if(!is.na(null.hypothesis)) {
      plo <- pbinom(y, n, null.hypothesis) - dbinom(y, n, null.hypothesis)/2
      phi <- 1 - plo + dbinom(y, n, null.hypothesis) / 2
      cdf <- pbinom(0:n, n, null.hypothesis) - dbinom(0:n, n, null.hypothesis) / 2
      if (plo < phi) {
        cdf <- 1 - cdf
        p2 <- plo
      } else p2 <- phi
      u <- cdf <= p2 * (1 + 1e-7)
      if(sum(u) > 0) p2 <- min(p2 + max(cdf[u]),1)
      if(test.type=="less") p.value <- plo
      else if(test.type=="greater") p.value <- phi
      else p.value <- p2
    } else p.value <- NA
    rslt <- c(n, y/n, NA, NA, plo, phi, p.value)
    names(rslt) <- c("n", "Est", "SE", "Statistic", "CIlo", "CIhi", "p.value")
    attr(rslt,"method") = "HalfP"
    rslt
  }

binomInference.jeffreys <-
  function(y, n=length(y), null.hypothesis=NA, test.type="two.sided", conf.level=0.95, version=F) {
    
    vrsn <- "20141002"
    if (version) return(vrsn)
    if(length(y) > 1){
      if(length(y) != n){
        stop("n must be equal to the length of y")
      }
    }
    y <- sum(y)
    if(y < 0 | y > n) stop("y must be an integer between 0 and n inclusive")
    alpha <- (1 - conf.level) / 2
    phat <- qbeta(0.5, y+0.5, n-y+0.5)
    if(y==0) elo <-0
    else {
      elo <- qbeta(alpha, y+0.5, n-y+0.5)
    }
    if(y==n) ehi <- 1
    else {
      ehi <- qbeta(1-alpha,y+0.5,n-y+0.5)
    }
    if(!is.na(null.hypothesis)) {
      plo <- pbeta(null.hypothesis, y+0.5, n-y+0.5)
      phi <- 1 - plo
      p2 <- 2 * min(plo,phi)
      if(test.type=="less") p.value <- plo
      else if(test.type=="greater") p.value <- phi
      else p.value <- p2
    } else p.value <- NA
    rslt <- c(n, phat, NA, NA, elo, ehi, p.value)
    names(rslt) <- c("n", "Est", "SE", "Statistic", "CIlo", "CIhi", "p.value")
    attr(rslt,"method") = "Jeffreys"
    rslt
  }

binomInference.score <-
  function(y, n=length(y), null.hypothesis=NA, test.type="two.sided", conf.level=0.95, version=F) {
    
    vrsn <- "20141002"
    if (version) return(vrsn)
    if(length(y) > 1){
      if(length(y) != n){
        stop("n must be equal to the length of y")
      }
    }
    y <- sum(y)
    if(y < 0 | y > n) stop("y must be an integer between 0 and n inclusive")
    alpha <- (1 - conf.level) / 2
    phat <- y / n
    if(y==0) slo <- 0
    else slo <- (2 * n * phat + qnorm(alpha)^2 - 
                   sqrt(4 * n * phat * (1 - phat) * qnorm(alpha)^2 + 
                          qnorm(alpha)^4)) / (2 * (n + qnorm(alpha)^2))
    if(y==n) shi <- 1
    else shi <- (2 * n * phat + qnorm(alpha)^2 +
                   sqrt(4 * n * phat * (1 - phat) * qnorm(alpha)^2 +
                          qnorm(alpha)^4)) / (2 * (n + qnorm(alpha)^2))
    if(!is.na(null.hypothesis)) {
      SE <- sqrt(null.hypothesis * (1 - null.hypothesis) / n)
      Z <- (phat - null.hypothesis) / SE
      plo <- pnorm((phat - null.hypothesis) / SE)
      phi <- 1 - pnorm((phat - null.hypothesis) / SE)
      p2 <- 2 * min(plo,phi)
      if(test.type=="less") p.value <- plo
      else if(test.type=="greater") p.value <- phi
      else p.value <- p2
    } else SE <- Z <- p.value <- NA
    rslt <- c(n, phat, SE, Z, slo, shi, p.value)
    names(rslt) <- c("n", "Est", "SE", "Statistic", "CIlo", "CIhi", "p.value")
    attr(rslt,"method") <- "Score"
    rslt
  }

binomInference.wald <-
  function(y, n=length(y), null.hypothesis=NA, test.type="two.sided", conf.level=0.95, version=F) {
    
    vrsn <- "20141002"
    if (version) return(vrsn)
    if(length(y) > 1){
      if(length(y) != n){
        stop("n must be equal to the length of y")
      }
    }
    y <- sum(y)
    if(y < 0 | y > n) stop("y must be an integer between 0 and n inclusive")
    alpha <- (1 - conf.level) / 2
    phat <- y / n
    wlo <- phat + qnorm(alpha)*sqrt(phat * (1-phat) / n)
    whi <- phat - qnorm(alpha)*sqrt(phat * (1-phat) / n)
    if(!is.na(null.hypothesis)) {
      SE <- sqrt(phat * (1 - phat) / n)
      Z <- (phat - null.hypothesis) / SE
      plo <- pnorm(Z)
      phi <- 1 - pnorm(Z)
      p2 <- 2 * min(plo,phi)
      if(test.type=="less") p.value <- plo
      else if(test.type=="greater") p.value <- phi
      else p.value <- p2
    } else SE <- Z <- p.value <- NA
    rslt <- c(n, phat, SE, Z, wlo, whi, p.value)
    names(rslt) <- c("n", "Est", "SE", "Statistic", "CIlo", "CIhi", "p.value")
    attr(rslt,"method") = "Wald"
    rslt
  }

KMinference.ident <-
  function (y)
    ciKM <- function (z, obs) {
      k <- sum (z$t <= obs)
      k <- c(z$S[k], z$lower[k], z$upper[k])
      names(k) <- c("Surv Prob", "Lower CI", "Upper CI")
      k
    }

CIefrKM <-
  function (z, obs, conf) {
    upper <- sqrt (z$v[sum (z$t <= obs)] / z$N) * z$S * qSupBrnMotn (conf)
    upper[z$t > obs] <- NA
    list (t=z$t, maxt=z$maxt, lower=z$S-upper, upper=z$S+upper, S=z$S)
  }

CIhwKM <-
  function (z, conf) {
    K <- z$v / (1 + z$v)
    upper <- z$S / sqrt (z$N) / (1 - K) * qSupBrnBrdg (conf)
    list (t=z$t, maxt=z$maxt, lower=z$S-upper, upper=z$S+upper, S=z$S)
  }

CIptKM <-
  function (z, conf) {
    upper <- sqrt (z$v / z$N) * z$S * qnorm ((1 + conf) / 2)
    list (t=z$t, maxt=z$maxt, lower=z$S-upper, upper=z$S+upper, S=z$S)
  }

qSupBrnMotn <-
  function (conf) ifelse(conf==.95,2.2414,NA)

qSupBrnBrdg <-
  function (conf) ifelse(conf==.95,1.3581,NA)

