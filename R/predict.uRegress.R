## A function to create prediction intervals for linear regression
## Created 20141216 by Brian Williamson

## Args: reg - "uRegress" object, returned by the regress() function
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