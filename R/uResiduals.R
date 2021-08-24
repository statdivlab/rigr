#' Extract Residuals from uRegress objects
#' 
#' Extracts residuals (unstandardized, standardized, studentized, or jackknife)
#' from \code{uRegress} objects.
#' 
#' Relies on
#' functionality from the \code{stats} package to return residuals from the
#' \code{uRegress} object. \code{"studentized"} residuals are computed as
#' internally studentized residuals, while \code{"jackknife"} computes the
#' externally studentized residuals.
#' 
#' @param object an object of class \code{uRegress}, as returned by
#' \link[rigr]{regress}.
#' @param type denotes the type of residuals to return. Default value is
#' \code{""}, which returns unstandardized residuals. \code{"standardized"},
#' \code{"studentized"}, and \code{"jackknife"} return the expected type of
#' residuals.
#' @param version if \code{TRUE}, the
#' version of the function will be returned. No other computations will be
#' performed.
#' @return Returns the type of residuals
#' requested. 
#' @seealso \code{\link[rigr]{regress}}, \code{\link[stats]{rstudent}},
#' \code{\link[stats]{rstandard}}
#' @examples
#' 
#' # Reading in a dataset
#' data(mri)
#' 
#' # Create a uRegress object, regressing ldl on age
#' ldlReg <- regress("mean", age~ldl, data=mri)
#' 
#' # Get the studentized residuals
#' uResiduals(ldlReg, "studentized")
#' 
#' # Get the jackknifed residuals
#' uResiduals(ldlReg, "jackknife")
#' 
#' @export uResiduals
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
