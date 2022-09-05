#' Tests of Linear Combinations of Regression Coefficients
#' 
#' Produces point estimates, interval estimates, and p-values for linear
#' combinations of regression coefficients using a \code{uRegress} object.
#' 
#' @aliases lincom lincom.do print.lincom
#' 
#' @param reg an object of class \code{uRegress}.
#' @param comb a vector or matrix containing the values of the constants which
#' create the linear combination of the form \deqn{c_0 + c_1\beta_1 + \dots}
#' Zeroes must be given if coefficients aren't going to be included. For testing
#' multiple combinations, this must be a matrix with number of columns equal to the number of
#' coefficients in the model.
#' @param null.hypoth the null hypothesis to compare the linear combination of
#' coefficients against. This is a scalar if one combination is given, and a 
#' vector or matrix otherwise. The default value is \code{0}. 
#' @param conf.level a number between 0 and 1, indicating the desired
#' confidence level for intervals.
#' @param robustSE a logical value indicating whether or not to use robust
#' standard errors in calculation. Defaults to \code{TRUE}. 
#' If \code{TRUE}, then \code{robustSE} must
#' have been \code{TRUE} when \code{reg} was created.
#' @param joint.test a logical value indicating whether or not to use a joint Chi-square test 
#' for all the null hypotheses. If joint.test is \code{TRUE}, then no confidence interval is calculated. 
#' Defaults to \code{FALSE}. 
#' @param useFdstn a logical indicator that the F distribution should be used for test statistics 
#' instead of the chi squared distribution. Defaults to \code{TRUE}. This option is not supported when 
#' input \code{reg} is a hazard regression (i.e., \code{fnctl="hazard"}).
#' @param eform a logical value indicating whether or not to exponentiate the
#' estimated coefficient. By default this is performed based on the type of
#' regression used.
#' 
#' @return A list of class \code{lincom} (\code{joint.test} is \code{False}) or 
#' \code{lincom.joint} (\code{joint.test} is \code{True}). For the \code{lincom} class,
#' \code{comb} entries in the list are labeled \code{comb1}, \code{comb2}, etc. for as many linear combinations were used. 
#' Each is a list with the following components:
#' \item{printMat}{A formatted table with inferential results for the linear combination of coefficients. 
#' These include the point estimate, standard error, confidence interval, and t-test for the linear 
#' combination.}
#' \item{nms}{The name of the linear combination, for printing.}
#' \item{null.hypoth}{The null hypothesis for the linear combination.}
#' 
#' @examples
#' # Loading required libraries
#' library(sandwich)
#' 
#' # Reading in a dataset
#' data(mri)
#' 
#' # Linear regression of LDL on age (with robust SE by default)
#' testReg <- regress ("mean", ldl~age+stroke, data = mri)
#' 
#' # Testing coefficient created by .5*age - stroke (the first 0 comes from excluding the intercept)
#' testC <- c(0, 0.5, -1)
#' lincom(testReg, testC)
#' 
#' # Test multiple combinations: 
#' # whether separately whether .5*age - stroke = 0 or Intercept + 60*age = 125 
#' testC <- matrix(c(0, 0.5, -1, 1, 60, 0), byrow = TRUE, nrow = 2)
#' lincom(testReg, testC, null.hypoth = c(0, 125))
#' 
#' # Test joint null hypothesis:
#' # H0: .5*age - stroke = 0 AND Intercept + 60*age = 125 
#' lincom(testReg, testC, null.hypoth = c(0, 125), joint.test = TRUE)
#' 
#' @export lincom
lincom <- function(reg, comb, null.hypoth=0, conf.level=.95, robustSE = TRUE, 
                   joint.test = FALSE, useFdstn = FALSE, eform=reg$fnctl!="mean"){
  ## if conf.level is not between 0 and 1, throw error
  if(conf.level < 0 || conf.level > 1){
    stop("Confidence Level must be between 0 and 1")
  }
  ## if reg is not a uRegress object, throw an error
  if(!("uRegress" %in% class(reg))){
    stop("uRegress object must be entered")
  }
  ## throw error if fnctl is survival and use F distribution is True
  if (reg$fnctl == "hazard") {
    if(useFdstn){
      warning("Cannot obtain an approximated denom df in F test; setting useFdstn=FALSE.")
      useFdstn = FALSE
    }
  }
  ## throw error if eform not logical
  if (!(is.logical(eform))){
    stop("Argument eform must be a logical.")
  }
  ## throw error if null.hypoth has any NAs
  if (sum(is.na(null.hypoth)) != 0 || sum(is.na(comb) != 0)){
    stop("comb' and 'null.hypoth' cannot contains NAs.")
  }
  
  lincom.do <- function(reg, comb, null.hypoth, conf.level, robustSE, eform){

    if(any(comb==0)){
      mat <- comb[-which(comb==0)]
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
        if (mat[i] > 0){
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
    
    tStat <- (newCoef-null.hypoth)/SE
    if (reg$fnctl != "hazard") {
      pval <- 2*stats::pt(-abs(tStat), df=reg$df[2]) ## return two sided test
      CIL <- newCoef - abs(stats::qt((1-conf.level)/2,df=reg$df[2])*SE)
      CIU <- newCoef + abs(stats::qt((1-conf.level)/2,df=reg$df[2])*SE)
    } else {
      pval <- 2*stats::pnorm(-abs(tStat)) ## return two sided test
      CIL <- newCoef - abs(stats::qnorm((1-conf.level)/2)*SE)
      CIU <- newCoef + abs(stats::qnorm((1-conf.level)/2)*SE)
    }

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
    
    lincom.obj <- list(printMat = printMat, nms = nms, null.hypoth = null.hypoth)
    invisible(lincom.obj)
  }

  lincom.do.joint <- function(reg, comb, null.hypoth, robustSE, useFdstn){
    
    rank_of_mat <- qr(comb)$rank
    new_coef <- comb %*%  reg$coefficients[,1,drop=FALSE] - null.hypoth
    if(robustSE){
      covMat <- comb %*% reg$robustCov %*% t(comb)
    } else {
      covMat <- comb %*% reg$naiveCov %*% t(comb)
    }
    if(useFdstn){
      test_stat <- c(t(new_coef) %*% solve(covMat) %*% new_coef)/rank_of_mat
      pval <- stats::pf(test_stat, rank_of_mat, reg$df[2], lower.tail=FALSE)
      printMat <- matrix(c(test_stat, rank_of_mat, reg$df[2],pval), nrow=1)
      dimnames(printMat) <- list(NULL, c("F stat","num df","den df","p value"))
    }else{
      test_stat <- c(t(new_coef) %*% solve(covMat) %*% new_coef)
      pval <- stats::pchisq(test_stat, df = rank_of_mat, lower.tail=FALSE)
      printMat <- matrix(c(test_stat, rank_of_mat, pval), nrow=1)
      dimnames(printMat) <- list(NULL, c("Chi2 stat","df","p value"))
    }
    lincom.obj <- list(printMat=printMat, null.hypoth=null.hypoth, nms=NULL)
    invisible(lincom.obj)
  }
  
  if(is.vector(comb)){
    ## if the number of coefficients listed is different from number in model (can actually be one more)
    if(length(comb) != dim(reg$coefficients)[1]){
      stop("Vector of constants must be equal to number of coefficients in model, including intercept if applicable")
    }
    ## throw error if null.hypoth is not a scalar
    if (!(is.numeric(null.hypoth)) || length(null.hypoth) > 1){
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
                            null.hypoth = null.hypoth, 
                            conf.level = conf.level, 
                            robustSE = robustSE, 
                            eform = eform)
    names(lincom.obj)[1] <- c("comb1")
    
  } else { ## it is a matrix
    if(dim(comb)[2]!=dim(reg$coefficients)[1]){
      stop("Matrix of constants must have columns equal to the number of coefficients")
    }
    if(is.vector(null.hypoth) && length(null.hypoth) > 1){
      null.hypoth <- matrix(null.hypoth, nrow=length(null.hypoth))
    } else{
      null.hypoth <- matrix(null.hypoth, nrow=dim(comb)[1])
    }
    ## throw error if null.hypoth has wrong dimension
    if (!(is.numeric(null.hypoth)) || dim(null.hypoth)[1] != dim(comb)[1] || dim(null.hypoth)[2] != 1){
      stop("Null hypothesis must numeric and of the same dimension as the number of combinations being tested.")
    }
    lincom.obj <- vector(mode = "list", length = dim(comb)[1])
    ## apply to a vector for each
    if(!joint.test){
      for(i in 1:dim(comb)[1]){
        lincom.obj.partial <- lincom.do(reg, comb[i,], null.hypoth[i,], conf.level, robustSE, eform)
        lincom.obj[[i]] <- lincom.obj.partial
        names(lincom.obj)[[i]] <- paste0("comb", i)
      }
    }else{
      # if it is joint test...
      lincom.obj <- lincom.do.joint(reg, comb, null.hypoth, robustSE, useFdstn)
    }
  }
  lincom.obj$call <- match.call()
  if(!joint.test){
    class(lincom.obj) <- "lincom"
  }else{
    class(lincom.obj) <- "lincom.joint"
  }
  return(lincom.obj)
  
}
