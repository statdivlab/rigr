## Extract Studentized Residuals from a uRegress object
## Created 20150116 by Brian Williamson
## Last modified 20150116 by Brian Williamson
## Relies on functionality from stats package

## Args: object - object returned from regress()
##       type - type of residuals; nothing, "standardized", "studentized", "jackknife"
uResiduals <- function(object, type="", version=FALSE){
  if(version){
    return("20150120")
  }
  if(!("uRegress" %in% class(object))){
    stop("The first argument entered must be a uRegress object.")
  }
  if(type != "" & !(type %in% c("standardized", "studentized", "jackknife"))){
    stop("The type of residual must either be not entered or must be standardized, studentized, or jackknife.")
  }
  ## if type is not entered, assume regular residuals
  if(type==""){
    return(object$residuals)
  }
  if(type=="jackknife"){
    return(rstudent(object$fit))
  }
  if(type=="studentized"){
    return(rstandard(object$fit))
  }
  if(type=="standardized"){
    sigmahat <- (sum(object$residuals^2))/(length(object$residuals)-object$df[1])
    return(object$residuals/(sigmahat))
  }
}