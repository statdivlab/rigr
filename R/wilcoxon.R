#' Wilcoxon Signed Rank and Mann-Whitney-Wilcoxon Rank Sum Test
#' 
#' Performs Wilcoxon signed rank test or Mann-Whitney-Wilcoxon rank sum test
#' depending on data and logicals entered. Relies heavily on the function
#' \code{\link[stats]{wilcox.test}}. Adds formatting and variances, and prints
#' the z-score and p-value in addition to the test statistic and p-value.
#' 
#' 
#' @aliases wilcoxon wilcoxon.do print.wilcoxon wilcoxon.default
#' 
#' @param y numeric vector of data values.
#' Non-finite (missing or infinite) values will be omitted.
#' @param x optional numeric vector of data
#' values. Non-finite (missing or infinite) values will be omitted.
#' @param alternative specifies the
#' alternative hypothesis for the test; acceptable values are
#' \code{"two.sided"}, \code{"greater"}, or \code{"less"}.
#' @param mu the value of the null
#' hypothesis.
#' @param paired logical indicating whether
#' the data are paired or not. Default is \code{FALSE}. If \code{TRUE}, data
#' must be the same length.
#' @param exact logical value indicating
#' whether or not an exact test should be computed.
#' @param correct logical indicating whether
#' or not a continuity correction should be used and displayed.
#' @param conf.int logical indicating whether
#' or not to calculate and display a 'confidence interval' (performs a
#' semi-parametric test on medians, and is non-robust) and point estimate.
#' @param conf.level confidence level for the
#' interval.
#' @param \dots only used in the generic S3 class.
#' 
#' @return A list of class \code{wilcoxon}
#' is returned. The print method lays out the information in an easy-to-read
#' format. \item{statistic}{the value of the test
#' statistic with a name describing it.} \item{parameter}{the parameter(s) for
#' the exact distribution of the test statistic.} \item{p.value}{the p-value
#' for the test (calculated for the test statistic).} \item{null.value}{the
#' parameter \code{mu}.} \item{alternative}{character string describing the
#' alternative hypothesis.} \item{method}{the type of test applied.}
#' \item{data.name}{a character string giving the names of the data.}
#' \item{conf.int}{a confidence interval for the location parameter (only
#' present if the argument \code{conf.int=TRUE}).} \item{estimate}{an estimate
#' of the location parameter (only present if the argument
#' \code{conf.int=TRUE}).} \item{table}{a formatted table of rank sum and
#' number of observation values, for printing.} \item{vars}{a formatted table
#' of variances, for printing.} \item{hyps}{a formatted table of the
#' hypotheses, for printing.} \item{inf}{a formatted table of inference values,
#' for printing.}
#' 
#' @seealso \code{\link[stats]{wilcox.test}}
#' @examples
#' 
#' #- Create the data -#
#' cf <- c(1153, 1132, 1165, 1460, 1162, 1493, 1358, 1453, 1185, 1824, 1793, 1930, 2075)
#' healthy <- c(996, 1080, 1182, 1452, 1634, 1619, 1140, 1123, 1113, 1463, 1632, 1614, 1836)
#' 
#' #- Perform the test -#
#' wilcoxon(cf, healthy, paired=TRUE)
#' 
#' #- Perform the test -#
#' wilcoxon(cf, healthy, conf.int=TRUE)
#' 
#' @export wilcoxon
wilcoxon <- function(y, x = NULL, alternative = "two.sided", 
                     mu = 0, paired = FALSE, exact = FALSE, correct = FALSE, conf.int = FALSE, 
                     conf.level = 0.95){
  wilcoxon.do <- function (y, x, alternative, 
                           mu, paired, exact, correct, conf.int, 
                           conf.level,...){
    # error handling for mu (null hypothesis mean)
    if (!missing(mu) && ((length(mu) > 1L) || !is.finite(mu))) 
      stop("'mu' must be a single number")
    # error handling for CI
    if (conf.int) {
      if (!((length(conf.level) == 1L) && is.finite(conf.level) && 
            (conf.level > 0) && (conf.level < 1))) 
        stop("'conf.level' must be a single number between 0 and 1")
    }
    # variables must be numeric and have same length (if two-sample)
    if (!is.numeric(y)) { 
      stop("'y' must be numeric")
    }
    if (!is.null(x)) {
      if (!is.numeric(x)) 
        stop("'x' must be numeric")
      DNAME <- paste(deparse(substitute(y, sys.frame(1))), "and", 
                     deparse(substitute(x, sys.frame(1)))) # data name
      if (paired) { # if paired, calculate differences
        if (length(y) != length(x)) 
          stop("'y' and 'x' must have the same length")
        OK <- complete.cases(y, x)
        y <- y[OK] - x[OK]
        x <- NULL
      }
      else { # only finite values allowed
        y <- y[is.finite(y)]
        x <- x[is.finite(x)]
      }
    }
    else {
      DNAME <- deparse(substitute(y, sys.frame(1))) # data name
      if (paired) 
        stop("'x' is missing for paired test")
      y <- y[is.finite(y)]
    }
    if (length(y) < 1L) 
      stop("not enough finite 'y' observations")
    
    # one-sample test
    CORRECTION <- 0
    if (is.null(x)) {
      METHOD <- "Wilcoxon signed rank test"
      y <- y - mu # shift by null mu
      # keep track of but drop zeroes
      ZEROES <- any(y == 0)
      nZeroes <- sum(y==0)
      sumZeroes <- 0
      if (ZEROES) 
        y <- y[y != 0]
      n <- as.double(length(y))
      # if user doesn't specify exact test, do exact automatically for < 50 obs
      # if (is.null(exact)) 
      #   exact <- (n < 50)
      r <- rank(abs(y))
      signs <- sign(y)
      nPos <- sum(signs>0)
      nNeg <- sum(signs<0)
      # sum ranks of pos and neg obs
      sumPos <- sum(r[y>0])
      sumNeg <- sum(r[y<0])
      STATISTIC <- setNames(sum(r[y > 0]), "V") # V statistic - sum ranks of pos obs
      TIES <- length(r) != length(unique(r))
      ePos <- n*(n+1)/4
      unadjVar <- (n*(n+1)*(2*n+1))/24
      zeroAdjVar <- (nZeroes*(nZeroes+1)*(2*nZeroes+1))/24
      if (exact && !TIES && !ZEROES) {# exact test + no adjustments
        METHOD <- sub("test", "exact test", METHOD, fixed = TRUE)
        tiedAdjVar <- 0
        adjVar <- unadjVar - zeroAdjVar
        PVAL <- switch(alternative, 
                       two.sided = {p <- if 
                          (STATISTIC > (n * (n + 1)/4)) psignrank(STATISTIC - 
                          1, n, lower.tail = FALSE) else psignrank(STATISTIC, n)
                          min(2 * p, 1)}, 
                       greater = psignrank(STATISTIC - 1, n, lower.tail = FALSE), 
                       less = psignrank(STATISTIC, n))
        z <- 2*(STATISTIC - ePos)/sqrt(n*(n+1)*(2*n+1)/6)
        # confidence interval
        if (conf.int) {
          y <- y + mu
          alpha <- 1 - conf.level
          diffs <- outer(y, y, "+")
          diffs <- sort(diffs[!lower.tri(diffs)])/2
          cint <- switch(alternative, two.sided = {
            qu <- qsignrank(alpha/2, n)
            if (qu == 0) qu <- 1
            ql <- n * (n + 1)/2 - qu
            achieved.alpha <- 2 * psignrank(trunc(qu) - 
                                              1, n)
            c(diffs[qu], diffs[ql + 1])
          }, greater = {
            qu <- qsignrank(alpha, n)
            if (qu == 0) qu <- 1
            achieved.alpha <- psignrank(trunc(qu) - 1, 
                                        n)
            c(diffs[qu], +Inf)
          }, less = {
            qu <- qsignrank(alpha, n)
            if (qu == 0) qu <- 1
            ql <- n * (n + 1)/2 - qu
            achieved.alpha <- psignrank(trunc(qu) - 1, 
                                        n)
            c(-Inf, diffs[ql + 1])
          })
          if (achieved.alpha - alpha > alpha/2) {
            warning("requested conf.level not achievable")
            conf.level <- 1 - signif(achieved.alpha, 2)
          }
          attr(cint, "conf.level") <- conf.level
          ESTIMATE <- c(`(pseudo)median` = median(diffs))
        }
      }
      else {# non-exact test and/or adjusting for ties
        NTIES <- table(r)
        z <- STATISTIC - n * (n + 1)/4
        SIGMA <- sqrt(n * (n + 1) * (2 * n + 1)/24 - sum(NTIES^3 - 
                                                           NTIES)/48)
        adjVar <- SIGMA^2
        tiedAdjVar <- adjVar - unadjVar - zeroAdjVar
        if (correct) { # continuity correction
          CORRECTION <- switch(alternative, two.sided = sign(z) * 
                                 0.5, greater = 0.5, less = -0.5)
          METHOD <- paste(METHOD, "with continuity correction")
        }
        z <- (z - CORRECTION)/SIGMA
        # normal approx for p-value
        PVAL <- switch(alternative, 
                       less = pnorm(z), 
                       greater = pnorm(z, lower.tail = FALSE), 
                       two.sided = 2 * min(pnorm(z), pnorm(z, lower.tail = FALSE)))
        if (conf.int) {# calculate confidence interval if requested
          y <- y + mu
          alpha <- 1 - conf.level
          mumin <- min(y)
          mumax <- max(y)
          wdiff <- function(d, zq) {
            yd <- y - d
            yd <- yd[yd != 0]
            ny <- length(yd)
            dr <- rank(abs(yd))
            zd <- sum(dr[yd > 0]) - ny * (ny + 1)/4
            NTIES.CI <- table(dr)
            SIGMA.CI <- sqrt(ny * (ny + 1) * (2 * ny + 
                                                1)/24 - sum(NTIES.CI^3 - NTIES.CI)/48)
            if (SIGMA.CI == 0) 
              stop("cannot compute confidence interval when all observations are tied", 
                   call. = FALSE)
            CORRECTION.CI <- if (correct) {
              switch(alternative, two.sided = sign(zd) * 
                       0.5, greater = 0.5, less = -0.5)
            }
            else 0
            (zd - CORRECTION.CI)/SIGMA.CI - zq
          }
          cint <- switch(alternative, two.sided = {
            repeat {
              mindiff <- wdiff(mumin, zq = qnorm(alpha/2, 
                                                 lower.tail = FALSE))
              maxdiff <- wdiff(mumax, zq = qnorm(alpha/2))
              if (mindiff < 0 || maxdiff > 0) alpha <- alpha * 
                2 else break
            }
            if (1 - conf.level < alpha * 0.75) {
              conf.level <- 1 - alpha
              warning("requested conf.level not achievable")
            }
            l <- uniroot(wdiff, c(mumin, mumax), tol = 1e-04, 
                         zq = qnorm(alpha/2, lower.tail = FALSE))$root
            u <- uniroot(wdiff, c(mumin, mumax), tol = 1e-04, 
                         zq = qnorm(alpha/2))$root
            c(l, u)
          }, greater = {
            repeat {
              mindiff <- wdiff(mumin, zq = qnorm(alpha, 
                                                 lower.tail = FALSE))
              if (mindiff < 0) alpha <- alpha * 2 else break
            }
            if (1 - conf.level < alpha * 0.75) {
              conf.level <- 1 - alpha
              warning("requested conf.level not achievable")
            }
            l <- uniroot(wdiff, c(mumin, mumax), tol = 1e-04, 
                         zq = qnorm(alpha, lower.tail = FALSE))$root
            c(l, +Inf)
          }, less = {
            repeat {
              maxdiff <- wdiff(mumax, zq = qnorm(alpha))
              if (maxdiff > 0) alpha <- alpha * 2 else break
            }
            if (1 - conf.level < alpha * 0.75) {
              conf.level <- 1 - alpha
              warning("requested conf.level not achievable")
            }
            u <- uniroot(wdiff, c(mumin, mumax), tol = 1e-04, 
                         zq = qnorm(alpha))$root
            c(-Inf, u)
          })
          attr(cint, "conf.level") <- conf.level
          correct <- FALSE
          ESTIMATE <- c(`(pseudo)median` = uniroot(wdiff, 
                                                   c(mumin, mumax), tol = 1e-04, zq = 0)$root)
        }
        if (exact && TIES) {
          warning("cannot compute exact p-value with ties")
          if (conf.int) 
            warning("cannot compute exact confidence interval with ties")
        }
        if (exact && ZEROES) {
          warning("cannot compute exact p-value with zeroes")
          if (conf.int) 
            warning("cannot compute exact confidence interval with zeroes")
        }
        if (exact && correct) {
          warnings("continuity correction does not apply to exact p-value")
        }
      }
    }
    # two sample test
    else { 
      if (length(x) < 1L) 
        stop("not enough finite 'x' observations")
      METHOD <- "Wilcoxon rank sum test"
      r <- rank(c(y - mu, x))
      n.y <- as.double(length(y))
      n.x <- as.double(length(x))
      rankSumX <- sum(r[seq(from=length(y)+1, to=length(y)+length(x))]) #- n.x * (n.x + 1)/2
      rankSumY <- sum(r[seq_along(y)]) #- n.y * (n.y + 1)/2
      unadjVar <- (n.y * n.x/12) * ((n.y + n.x + 1))
      expectedY <- n.y*(n.y + n.x + 1)/2#(rankSumX+rankSumY)/2
      expectedX <- n.x*(n.y + n.x + 1)/2#(rankSumX+rankSumY)/2
      # if (is.null(exact)) 
      #   exact <- (n.y < 50) && (n.x < 50)
      STATISTIC <- c(W = sum(r[seq_along(y)]) - n.y * (n.y + 
                                                         1)/2)
      TIES <- (length(r) != length(unique(r)))
      if (exact && !TIES) { #no need for correction
        METHOD <- sub("test", "exact test", METHOD, fixed = TRUE)
        tiedAdjVar <- 0
        adjVar <- unadjVar
        z <- STATISTIC - n.y * n.x/2
        if (correct) {
          CORRECTION <- switch(alternative, two.sided = sign(z) * 
                                 0.5, greater = 0.5, less = -0.5)
          METHOD <- paste(METHOD, "with continuity correction")
        }
        z <- (z - CORRECTION)/sqrt(adjVar)
        
        PVAL <- switch(alternative, two.sided = {
          p <- if (STATISTIC > (n.y * n.x/2)) pwilcox(STATISTIC - 
                                                        1, n.y, n.x, lower.tail = FALSE) else pwilcox(STATISTIC, 
                                                                                                      n.y, n.x)
          min(2 * p, 1)
        }, greater = {
          pwilcox(STATISTIC - 1, n.y, n.x, lower.tail = FALSE)
        }, less = pwilcox(STATISTIC, n.y, n.x))
        if (conf.int) {
          alpha <- 1 - conf.level
          diffs <- sort(outer(y, x, "-"))
          cint <- switch(alternative, two.sided = {
            qu <- qwilcox(alpha/2, n.y, n.x)
            if (qu == 0) qu <- 1
            ql <- n.y * n.x - qu
            achieved.alpha <- 2 * pwilcox(trunc(qu) - 1, 
                                          n.y, n.x)
            c(diffs[qu], diffs[ql + 1])
          }, greater = {
            qu <- qwilcox(alpha, n.y, n.x)
            if (qu == 0) qu <- 1
            achieved.alpha <- pwilcox(trunc(qu) - 1, n.y, 
                                      n.x)
            c(diffs[qu], +Inf)
          }, less = {
            qu <- qwilcox(alpha, n.y, n.x)
            if (qu == 0) qu <- 1
            ql <- n.y * n.x - qu
            achieved.alpha <- pwilcox(trunc(qu) - 1, n.y, 
                                      n.x)
            c(-Inf, diffs[ql + 1])
          })
          if (achieved.alpha - alpha > alpha/2) {
            warning("Requested conf.level not achievable")
            conf.level <- 1 - achieved.alpha
          }
          attr(cint, "conf.level") <- conf.level
          ESTIMATE <- c(`difference in location` = median(diffs))
        }
      }
      else { #need to adjust for ties
        NTIES <- table(r)
        tiedAdjVar <- sum(NTIES^3 - NTIES)/((n.y + n.x) * (n.y + n.x - 1))
        adjVar <- unadjVar - tiedAdjVar
        z <- STATISTIC - n.y * n.x/2
        SIGMA <- sqrt((n.y * n.x/12) * ((n.y + n.x + 1) - 
                                          sum(NTIES^3 - NTIES)/((n.y + n.x) * (n.y + n.x - 
                                                                                 1))))
        if (correct) {
          CORRECTION <- switch(alternative, two.sided = sign(z) * 
                                 0.5, greater = 0.5, less = -0.5)
          METHOD <- paste(METHOD, "with continuity correction")
        }
        z <- (z - CORRECTION)/SIGMA
        PVAL <- switch(alternative, 
                       less = pnorm(z), 
                       greater = pnorm(z, lower.tail = FALSE), 
                       two.sided = 2 * min(pnorm(z), pnorm(z, lower.tail = FALSE)))
        if (conf.int) {
          alpha <- 1 - conf.level
          mumin <- min(y) - max(x)
          mumax <- max(y) - min(x)
          wdiff <- function(d, zq) {
            dr <- rank(c(y - d, x))
            NTIES.CI <- table(dr)
            dz <- (sum(dr[seq_along(y)]) - n.y * (n.y + 
                                                    1)/2 - n.y * n.x/2)
            CORRECTION.CI <- if (correct) {
              switch(alternative, two.sided = sign(dz) * 
                       0.5, greater = 0.5, less = -0.5)
            }
            else 0
            SIGMA.CI <- sqrt((n.y * n.x/12) * ((n.y + n.x + 
                                                  1) - sum(NTIES.CI^3 - NTIES.CI)/((n.y + n.x) * 
                                                                                     (n.y + n.x - 1))))
            if (SIGMA.CI == 0) 
              stop("cannot compute confidence interval when all observations are tied", 
                   call. = FALSE)
            (dz - CORRECTION.CI)/SIGMA.CI - zq
          }
          root <- function(zq) {
            f.lower <- wdiff(mumin, zq)
            if (f.lower <= 0) 
              return(mumin)
            f.upper <- wdiff(mumax, zq)
            if (f.upper >= 0) 
              return(mumax)
            uniroot(wdiff, c(mumin, mumax), f.lower = f.lower, 
                    f.upper = f.upper, tol = 1e-04, zq = zq)$root
          }
          cint <- switch(alternative, two.sided = {
            l <- root(zq = qnorm(alpha/2, lower.tail = FALSE))
            u <- root(zq = qnorm(alpha/2))
            c(l, u)
          }, greater = {
            l <- root(zq = qnorm(alpha, lower.tail = FALSE))
            c(l, +Inf)
          }, less = {
            u <- root(zq = qnorm(alpha))
            c(-Inf, u)
          })
          attr(cint, "conf.level") <- conf.level
          correct <- FALSE
          ESTIMATE <- c(`difference in location` = uniroot(wdiff, 
                                                           c(mumin, mumax), tol = 1e-04, zq = 0)$root)
        }
        if (exact && TIES) {
          warning("cannot compute exact p-value with ties")
          if (conf.int) 
            warning("cannot compute exact confidence intervals with ties")
        }
      }
    }

    names(mu) <- if (paired || !is.null(x)) 
      "location shift"
    else "location"
    RVAL <- list(statistic = STATISTIC, parameter = NULL, p.value = as.numeric(PVAL), 
                 null.value = mu, alternative = alternative, method = METHOD, 
                 data.name = DNAME)
    if("signed" %in% strsplit(METHOD, " ")[[1]]){# create formatted matrices for printing signed rank test
      ## create matrix of observed and expected values
      printMat <- matrix(c(nPos, sumPos, ePos), nrow=1)
      colnames(printMat) <- c("obs", "sum ranks", "expected")
      printMat <- rbind(printMat, matrix(c(nNeg, sumNeg, ePos), nrow=1))
      printMat <- rbind(printMat, matrix(c(nZeroes, sumZeroes, 0), nrow=1))
      printMat <- rbind(printMat, apply(printMat, 2, sum))
      rownames(printMat) <- c("positive", "negative", "zero", "all")
      ## matrix of variances
      vars <- matrix(c(unadjVar, tiedAdjVar, zeroAdjVar, adjVar), ncol=1)
      colnames(vars) <- " "
      rownames(vars) <- c("unadjusted variance", 
                          "adjustment for ties", 
                          "adjustment for zeroes", 
                          "adjusted variance")
      ## format z and pvalue
      if(conf.int){
        inf <- matrix(c(STATISTIC, format(as.numeric(PVAL), digits=5), 
                        paste("[", cint[1],", ", cint[2],"]", sep=""), ESTIMATE), nrow=1)
        # if exact test, do not calculate z-score or corresponding p
        if (!("exact" %in% strsplit(METHOD, " ")[[1]])){
          inf <- rbind(inf, 
                       matrix(c(format(z, digits=5), 
                                format(switch(alternative,less=pnorm(z), greater=1-pnorm(z), 
                                              two.sided=2*min(pnorm(z),1-pnorm(z))), digits = 5),
                                " ", " "), 
                              nrow=1))
        }
        colnames(inf) <- c("Test Statistic", "p-value", "CI", "Point Estimate")
      } else {
        inf <- matrix(c(STATISTIC, format(as.numeric(PVAL), digits=5)), nrow=1)
        if (!("exact" %in% strsplit(METHOD, " ")[[1]])){
          inf <- rbind(inf, 
                       matrix(c(format(z, digits=5), 
                                format(switch(alternative,
                                              less=pnorm(z), 
                                              greater=1-pnorm(z), 
                                              two.sided=2*min(pnorm(z),1-pnorm(z))), 
                                       digits = 5)), 
                              nrow=1))
        }
        colnames(inf) <- c("Test Statistic", "p-value")
      }
      if ("exact" %in% strsplit(METHOD, " ")[[1]]){
        rownames(inf) <- c(names(STATISTIC))
      } else{
        rownames(inf) <- c(names(STATISTIC), "Z")
      }
      hyps <- matrix(c(mu, alternative),nrow=1)
      colnames(hyps) <- c("H0", "Ha")
      rownames(hyps) <- "Hypothesized Median"
    } else{ # formatted matrices for printing rank sum test
      printMat <- matrix(c(n.y, rankSumY, expectedY), nrow=1)
      colnames(printMat) <- c("obs", "rank sum", "expected")
      printMat <- rbind(printMat, matrix(c(n.x, rankSumX, expectedX), nrow=1))
      printMat <- rbind(printMat, apply(printMat, 2, sum))
      rownames(printMat) <- c("Y", "X", "combined")
      ## matrix of variances
      vars <- matrix(c(unadjVar, tiedAdjVar, adjVar), ncol=1)
      colnames(vars) <- " "
      rownames(vars) <- c("unadjusted variance", "adjustment for ties", "adjusted variance")
      ## format z and pvalue
      if(conf.int){
        inf <- matrix(c(STATISTIC, format(as.numeric(PVAL), digits=5), 
                        paste("[", cint[1],", ", cint[2],"]", sep=""), ESTIMATE), nrow=1)
        if (!("exact" %in% strsplit(METHOD, " ")[[1]])){
          inf <- rbind(inf, 
                       matrix(c(format(z, digits=5),
                                format(switch(alternative,
                                              less=pnorm(z), 
                                              greater=1-pnorm(z), 
                                              two.sided=2*min(pnorm(z),1-pnorm(z))), 
                                       digits=5), " ", " "), 
                              nrow=1))
        }
        colnames(inf) <- c("Test Statistic", "p-value", "CI", "Point Estimate")
      } else {
        inf <- matrix(c(STATISTIC, format(as.numeric(PVAL), digits=5)), nrow=1)
        if (!("exact" %in% strsplit(METHOD, " ")[[1]])){
          inf <- rbind(inf, 
                       matrix(c(format(z, digits=5), 
                                format(switch(alternative,
                                              less=pnorm(z), 
                                              greater=1-pnorm(z), 
                                              two.sided=2*min(pnorm(z),1-pnorm(z))), 
                                       digits = 5)), 
                              nrow=1))
        }
        colnames(inf) <- c("Test Statistic", "p-value")
      }
      if ("exact" %in% strsplit(METHOD, " ")[[1]]){
        rownames(inf) <- c(names(STATISTIC))
      } else{
        rownames(inf) <- c(names(STATISTIC), "Z")
      }
      hyps <- matrix(c(mu, alternative),nrow=1)
      colnames(hyps) <- c("H0", "Ha")
      rownames(hyps) <- "Hypothesized Median"
    }
    RVAL <- c(RVAL, list(table=printMat, vars=vars, hyps=hyps, inf=inf))
    
    if (conf.int) 
      RVAL <- c(RVAL, list(conf.int = cint, estimate = ESTIMATE))
    invisible(RVAL)
  }
  
  wilcox.obj <- wilcoxon.do(y=y,
                            x=x,
                            alternative=alternative,
                            mu=mu,paired=paired,
                            exact=exact,
                            correct=correct,
                            conf.int=conf.int,
                            conf.level=conf.level)
  wilcox.obj$call <- match.call()
  class(wilcox.obj) <- "wilcoxon"
  return(wilcox.obj)
}

