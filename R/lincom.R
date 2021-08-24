#' Tests of Linear Combinations of Regression
#' Coefficients
#' 
#' Produces point estimates, interval estimates, and p values for linear
#' combinations of regression coefficients using a \code{ uRegress} object.
#' 
#' @aliases lincom lincom.do print.lincom
#' 
#' @param reg an object of class \code{uRegress}.
#' @param comb a vector or matrix containing the values of the constants which
#' create the linear combination of the form \deqn{c_0 + c_1\beta_1 + \dots}.
#' need to give zeroes if coefficients aren't going to be included
#' @param hyp the null hypothesis to compare the linear combination of
#' coefficients against. The default value is \code{0}. An error will be thrown
#' if the number of columns of this matrix are not equal to the number of
#' coefficients in the model. a matrix (or single) hypothesis
#' @param conf.level a number between 0 and 1, indicating the desired
#' confidence level for intervals.
#' @param robustSE a logical value indicating whether or not to use robust
#' standard errors in calculation. If \code{TRUE}, then \code{robustSE} must
#' have been \code{TRUE} when \code{reg} was created.
#' @param eform a logical value indicating whether or not to exponentiate the
#' estimated coefficient. By default this is performed based on the type of
#' regression used.
#' @return Prints a matrix with the point
#' estimate of the linear combination of coefficients, a p-value, and
#' confidence interval.
#' @author Scott S. Emerson, M.D., Ph.D., Andrew J. Spieker,
#' Brian D. Williamson
#' @examples
#' 
#' # Loading required libraries
#' library(survival)
#' library(sandwich)
#' 
#' # Reading in a dataset
#' data(mri)
#' 
#' # Linear regression of LDL on age (with robust SE by default)
#' testReg <- regress ("mean", ldl~age+stroke, data = mri)
#' 
#' # Testing coefficient created by .5*age - stroke (the first 1 comes from including the intercept)
#' testC <- c(1, .5, -1)
#' lincom(testReg, testC)
#' 
#' @export lincom
lincom <- function(reg, comb, hyp=0, conf.level=.95, robustSE = TRUE, eform=reg$fnctl!="mean"){
  ## if conf.level is not between 0 and 1, throw error
  if(conf.level < 0 || conf.level > 1){
    stop("Confidence Level must be between 0 and 1")
  }
  ## if reg is not a uRegress object, throw an error
  if(!("uRegress" %in% class(reg))){
    stop("uRegress object must be entered")
  }
  ## throw error if eform not logical
  if (!(is.logical(eform))){
    stop("Argument eform must be a logical.")
  }
  
  lincom.do <- function(reg, comb, hyp, conf.level, robustSE, eform){

    if(any(comb==0)){
      mat <- comb[-(comb==0)]
    } else {
      mat <- comb
    }
    comb <- matrix(comb, nrow=1)
    getNms <- ifelse(comb==0, FALSE, TRUE)
    nms <- dimnames(reg$coefficients)[[1]][getNms]
    # this had >0 previously and a hard-coded + sign in the names - why??
    nms <- paste(mat[mat!=0], nms, sep="*")
    if(length(nms)>1){
      for (i in 2:length(nms)){
        if (getNms[i] && comb[1,i] > 0){
          nms[i] <- paste0("+", nms[i])
        }
      }
      nms <- paste(nms, collapse="")
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
    pval <- 2*pt(-abs(tStat), reg$df[2]) ## return two sided test
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
    
    lincom.obj <- list(printMat = printMat, nms = nms, hyp = hyp)
    invisible(lincom.obj)
  }
  
  if(is.vector(comb)){
    ## if the number of coefficients listed is different from number in model (can actually be one more)
    if(length(comb) != dim(reg$coefficients)[1]){
      stop("Vector of constants must be equal to number of coefficients in model, including intercept if applicable")
    }
    ## throw error if hyp is not a scalar
    if (!(is.numeric(hyp)) || length(hyp) > 1){
      stop("Null hypothesis must a scalar.")
    }
    ## if robustSE requested but was not in regress(), throw error
    if(is.null(reg$robustCov)){
      if(robustSE){
        stop("uRegress object must be created with robust standard errors")
      }
    }
    lincom.obj <- vector(mode = "list", length = 1)
    lincom.obj[[1]] <- lincom.do(reg = reg, 
                            comb = comb, 
                            hyp = hyp, 
                            conf.level = conf.level, 
                            robustSE = robustSE, 
                            eform = eform)
    names(lincom.obj)[1] <- c("comb1")
    
  } else { ## it is a matrix
    if(dim(comb)[2]!=dim(reg$coefficients)[1]){
      stop("Matrix of constants must have columns equal to the number of coefficients")
    }
    if(is.vector(hyp)){
      hyp <- matrix(hyp, nrow=dim(comb)[1])
    }
    ## throw error if hyp has wrong dimension
    if (!(is.numeric(hyp)) || dim(hyp)[1] != dim(comb)[1] || dim(hyp)[2] != 1){
      stop("Null hypothesis must numeric and of the same dimension as the number of combinations being tested.")
    }
    lincom.obj <- vector(mode = "list", length = dim(comb)[1])
    ## apply to a vector for each
    for(i in 1:dim(comb)[1]){
      lincom.obj.partial <- lincom.do(reg, comb[i,], hyp[i,], conf.level, robustSE, eform)
      lincom.obj[[i]] <- lincom.obj.partial
      names(lincom.obj)[[i]] <- paste0("comb", i)
    }
  }
  lincom.obj$call <- match.call()
  class(lincom.obj) <- "lincom"
  return(lincom.obj)
  
}
