#' Prediction Intervals for \code{uRegress} objects
#' 
#' Produces prediction intervals for objects of class \code{uRegress}.
#' 
#' 
#' @aliases predict.uRegress predict
#' @param object an object of class \code{uRegress}.
#' @param interval Type of interval calculation
#' @param level Tolerance/confidence level
#' @param ...  other arguments to pass to the appropriate predict function for
#' the class of \code{object$fit}. See \code{\link[survival]{predict.coxph}},
#' \code{\link[stats]{predict.lm}}, or \code{\link[stats]{predict.glm}} for
#' more details. Predictions are not currently implemented for objects of type
#' \code{\link[geepack]{geeglm}}.
#' @return %% ~Describe the value returned Returns a matrix with the fitted
#' value and prediction interval for the entered X. 
#' @seealso \code{\link[uwIntroStats]{regress}}
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
#' testReg <- regress ("mean", ldl~age)
#' 
#' # 95% Prediction Interval for age 50
#' predict(testReg)
#' 
#' @export predict.uRegress
predict.uRegress <- function(object, interval="prediction",level=0.95,...){
  
  ## if reg is not a uRegress object, throw an error
  if(!("uRegress" %in% class(object))){
    stop("uRegress object must be entered")
  }
  if(class(object$fit)=="lm"){
    ret <- predict.lm(object$fit,interval=interval,level=level, ...)
  } else if(class(object$fit)[1]=="glm"){
    ret <- predict.glm(object$fit,interval=interval,level=level,...)
  } else if (class(object$fit)[1]=="coxph"){
    ret <- predict(object$fit,interval=interval,level=level, ...)
  } else {
    stop("Predictions not yet implemented for objects of type 'geeglm'")
  }
  return(ret)
}
