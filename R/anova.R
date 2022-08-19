#' ANOVA 
#' 
#' Compute analysis of variance (or deviance) tables for two fitted, nested \code{uRegress} objects. The model with more
#' parameters is referred to as the full model (or the larger model), and the model with fewer
#' parameters is referred to as the null model (or the smaller model).
#' 
#' @param object an object of class \code{uRegress}, the model with fewer parameters (i.e. the null model).
#' @param full_object an object of class \code{uRegress}, the model with more parameters (i.e. the full model).
#' @param test a character string specifying the test statistic to be used. Can be one of 'Wald' or 'LRT'.
#' @param robustSE a logical value indicating whether or not to use robust
#' standard errors in calculation. Defaults to \code{TRUE}. 
#' If \code{TRUE}, then \code{robustSE} must
#' have been \code{TRUE} when \code{reg} was created.
#' @param useFdstn a logical indicator that the F distribution should be used for test statistics 
#' instead of the chi squared distribution. Defaults to \code{TRUE}. 
#' @param ... argument to be passed in
#' @return A list of class \code{anova.uRegress} with the following components:
#' \item{printMat}{A formatted table with inferential results (i.e., test statistics and p-values) for comparing two nested models.}
#' \item{null model}{The null model in the comparison.}
#' \item{full model}{The full model in the comparison.}

#' @examples
#' # Loading required libraries
#' library(sandwich)
#' 
#' # Reading in a dataset
#' data(mri)
#' 
#' # Linear regression of LDL on age and stroke (with robust SE by default)
#' testReg_null <- regress ("mean", ldl~age+stroke, data = mri)
#' 
#' # Linear regression of LDL on age, stroke, and race (with robust SE by default)
#' testReg_full <- regress ("mean", ldl~age+stroke+race, data = mri)

#' # Comparing the two models using the Wald test with robust SE
#' anova(testReg_null, testReg_full, test = "Wald")
#' @rdname anova.uRegress
#' @export 
anova.uRegress <- function(object, full_object, test="Wald", robustSE = TRUE, useFdstn = TRUE, ...){
  
  if(!("uRegress" %in% class(object))|!("uRegress" %in% class(full_object))){
    stop("uRegress objects must be entered!")
  }
  
  if(!(test %in% c("Wald","LRT"))){
    stop("Only Wald and LRT tests are available.")
  }
  
  if(is.null(object$robustCov)|is.null(full_object$robustCov)){
    if(robustSE){
      stop("uRegress objects must be created with robust standard errors.")
    }
  }
  # simple check for nested models
  if(full_object$fit$rank<=object$fit$rank){
    warning("The null model has the same or more coefficients than the full model. We will proceed 
    by reversing their orders. Double check the order of the two models passed in!")
    temp_object <- full_object
    full_object <- object
    object <- temp_object
  }
  
  object_cov <- (rownames(object$coefficients))
  full_object_cov <- (rownames(full_object$coefficients))
  
  if(!(all(object_cov %in% object_cov))){
    warning("The two models do not appear to be nested -- double check the input.")
    if(test!="LRT"){
      stop("Only LRT is supported when the input models do not appear to be nested.")
    }
  }
  
  # first do likelihood ratio test 
  if(test=="LRT"){
    # we first extract the likelihood of both models
    lr_null <- stats::logLik(object$fit)[1]
    lr_full <- stats::logLik(full_object$fit)[1]
    df <- abs(full_object$fit$rank - object$fit$rank)
    LRStat <- 2 * abs(lr_full - lr_null)
    # compute p-value
    pval <- pchisq(LRStat, df, lower.tail = FALSE)
    printMat <- matrix(c(LRStat, df, pval), nrow=1)
    dimnames(printMat) <- list(NULL, c("Chi2 stat","df","p value"))
  }else if(test=="Wald"){
    # we first need to match the coefficients at the full model
    comb <- diag(nrow(full_object$coefficients))
    # go through each position 
    for (i in seq_along(object_cov)){
      # match one position
      curr_coef <- object_cov[i]
      pos_of_coef <- which(object_cov==curr_coef)
      comb[pos_of_coef,pos_of_coef] <- 0
    }
    comb <- comb[rowSums(comb)>0,]
    new_coef <- comb %*% full_object$coefficients[,1,drop=FALSE]
    if(robustSE){
      covMat <- comb %*% full_object$robustCov %*% t(comb)
    } else {
      covMat <- comb %*% full_object$naiveCov %*% t(comb)
    }
    if(useFdstn){
      test_stat <- c(t(new_coef) %*% solve(covMat) %*% new_coef)/nrow(comb)
      pval <- stats::pf(test_stat, nrow(comb), full_object$df[2], lower.tail=FALSE)
      printMat <- matrix(c(test_stat, nrow(comb), full_object$df[2],pval), nrow=1)
      dimnames(printMat) <- list(NULL, c("F stat","num df","den df","p value"))
    }else{
      test_stat <- c(t(new_coef) %*% solve(covMat) %*% new_coef)
      pval <- stats::pchisq(test_stat, df = nrow(comb), lower.tail=FALSE)
      printMat <- matrix(c(test_stat, nrow(comb), pval), nrow=1)
      dimnames(printMat) <- list(NULL, c("Chi2 stat","df","p value"))
    }
  }
  anova.object <- list(printMat=printMat, object = formula(object$fit), 
                       full_object = formula(full_object$fit))
  anova.object$call <- match.call()
  class(anova.object) <- "anova.uRegress"
  return(anova.object)
}
  
  
