## Equivalent function to "lincom" in STATA
## Designed to find relationships in linear combinations of estimators following regress() call
## Created 20141216 by Brian Williamson

## Args:    reg - a "uRegress" object
##         comb - a vector of constants (can be positive or negative) to construct linear combination
##             need to give zeroes if coefficients aren't going to be included
##          hyp - a matrix (or single) hypothesis
##   conf.level - confidence level (between 0 and 1, default is .95)
##     robustSE - logical, indicating if robust standard errors are to be used
##        eform - logical, whether to return exponentiated coefficients or not
## Returns: estimate of the linear combination of coefficients, specified by the comb vector
## Version: 20150604




#' %% ~~function to do ... ~~ Tests of Linear Combinations of Regression
#' Coefficients
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' Produces point estimates, interval estimates, and p values for linear
#' combinations of regression coefficients using a \code{ uRegress} object.
#' 
#' 
#' @param reg an object of class \code{uRegress}.
#' @param comb a vector or matrix containing the values of the constants which
#' create the linear combination of the form \deqn{c_0 + c_1\beta_1 + \dots}.
#' @param hyp the null hypothesis to compare the linear combination of
#' coefficients against. The default value is \code{0}. An error will be thrown
#' if the number of columns of this matrix are not equal to the number of
#' coefficients in the model.
#' @param conf.level a number between 0 and 1, indicating the desired
#' confidence level for intervals.
#' @param robustSE a logical value indicating whether or not to use robust
#' standard errors in calculation. If \code{TRUE}, then \code{robustSE} must
#' have been \code{TRUE} when \code{reg} was created.
#' @param eform a logical value indicating whether or not to exponentiate the
#' estimated coefficient. By default this is performed based on the type of
#' regression used.
#' @return %% ~Describe the value returned Prints a matrix with the point
#' estimate of the linear combination of coefficients, a p-value, and
#' confidence interval.
#' @author %% ~~who you are~~ Scott S. Emerson, M.D., Ph.D., Andrew J. Spieker,
#' Brian D. Williamson
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' # Loading required libraries
#' library(survival)
#' library(sandwich)
#' 
#' # Reading in a dataset
#' mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt",header=TRUE)
#' attach(mri)
#' 
#' # Linear regression of LDL on age (with robust SE by default)
#' testReg <- regress ("mean", ldl~age+stroke)
#' 
#' # Testing coefficient created by .5*age - stroke (the first 1 comes from including the intercept)
#' testC <- c(1, .5, -1)
#' lincom(testReg, testC)
#' 
#' @export lincom
lincom <- function(reg, comb, hyp=0, conf.level=.95, robustSE = TRUE, eform=reg$fnctl!="mean"){
  ## if conf.level is not between 0 and 1, throw error
  if(conf.level < 0 | conf.level > 1){
    stop("Confidence Level must be between 0 and 1")
  }
  ## if reg is not a uRegress object, throw an error
  if(!("uRegress" %in% class(reg))){
    stop("uRegress object must be entered")
  }
  
  if(is.vector(comb)){
    ## if the number of coefficients listed is different from number in model (can actually be one more)
    if(length(comb) != dim(reg$coefficients)[1]){
      stop("Vector of constants must be equal to number of coefficients in model")
    }
    if(any(comb==0)){
      mat <- comb[-(comb==0)]
    } else {
      mat <- comb
    }
    comb <- matrix(comb, nrow=1)
    getNms <- ifelse(comb==0, FALSE, TRUE)
    nms <- dimnames(reg$coefficients)[[1]][getNms]
    nms <- paste(mat[mat>0], nms, sep="*")
    if(length(nms)>1){
      nms <- paste(nms, collapse="+")
    }
    ## if robustSE requested but was not in regress(), throw error
    if(is.null(reg$robustCov)){
      if(robustSE){
        stop("uRegress object must be created with robust standard errors")
      }
    }
    
    ## create new coefficient 
    newCoef <- comb%*%reg$coefficients[,1,drop=FALSE]
    
    if(robustSE){
      SE <- suppressWarnings(sqrt(comb%*%reg$robustCov%*%t(comb)))
    } else {
      SE <- suppressWarnings(sqrt(comb%*%reg$naiveCov%*%t(comb)))
    }
    if(dim(SE)[2]>1){
      SE <- matrix(apply(SE, 1, function(x) x[!is.na(x)]), nrow=dim(SE)[2])
    }
    
    
    tStat <- (newCoef-hyp)/SE
    pval <- 2*pt(-abs(tStat), reg$df[2]) ## return two sided tes
    CIL <- newCoef - abs(qt((1-conf.level)/2,df=reg$df[2])*SE)
    CIU <- newCoef + abs(qt((1-conf.level)/2,df=reg$df[2])*SE)
    
    if(eform){
      CIL <- exp(CIL)
      CIU <- exp(CIU)
      newCoef <- exp(comb%*%reg$coefficients[,1,drop=FALSE])
    }
    
    printMat <- matrix(c(newCoef, SE, CIL, CIU, tStat, pval), nrow=dim(comb)[1])
    dimnames(printMat) <- list(rep("", dim(comb)[1]), c("Estimate", "Std. Err.",
                                                        paste(format(100*conf.level),"%",c("L","H"), sep=""),
                                                        "T", "Pr(T > |t|)"))
    if(eform){
      dimnames(printMat) <- list("", c("e(Est)", "Std. Err.",
                                       paste("e(", paste(format(100*conf.level),"%",c("L","H"),sep=""), ")",sep=""),
                                       "T", "Pr(T > |t|)"))
    }
    ## print
    for(i in 1:dim(printMat)[1]){
      cat("\nH0:", nms[i], "  = ", hyp, "\n")
      cat("Ha:", nms[i], " != ", hyp, "\n")
      printCoefmat(t(printMat[i,]), digits = 4, 
                   has.Pvalue = TRUE)
    }
    
  } else { ## it is a matrix
    if(dim(comb)[2]!=dim(reg$coefficients)[1]){
      stop("Matrix of constants must have columns equal to the number of coefficients")
    }
    if(is.vector(hyp)){
      hyp <- matrix(hyp, nrow=dim(comb)[1])
    }
    ## apply to a vector for each
    for(i in 1:dim(comb)[1]){
      lincom(reg, comb[i,], hyp[i,], conf.level, robustSE, eform)
    }
    ## Do overall test
    overall <- apply(comb, 2, sum)
    cat("\n Overall Test \n")
    lincom(reg, overall, 0, conf.level, robustSE, eform)
  }
}
