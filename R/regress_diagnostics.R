#' Extract Residuals from \code{uRegress} objects
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
#' @aliases residuals.uRegress residuals
#' 
#' @param object an object of class \code{uRegress}, as returned by
#' \link[rigr]{regress}.
#' @param type denotes the type of residuals to return. Default value is
#' \code{""}, which returns unstandardized residuals. \code{"standardized"},
#' \code{"studentized"}, and \code{"jackknife"} return the expected type of
#' residuals.
#' @param ...  other arguments 
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
#' residuals(ldlReg, "studentized")
#' 
#' # Get the jackknifed residuals
#' residuals(ldlReg, "jackknife")
#' 
#' @export
residuals.uRegress <- function(object, type="", ...){
  
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
    return(stats::rstudent(object$fit))
  }
  if(type=="studentized"){
    return(stats::rstandard(object$fit))
  }
  if(type=="standardized"){
    sigmahat <- (sum(object$residuals^2))/(length(object$residuals)-object$df[1])
    return(object$residuals/(sigmahat))
  }
}

#' @export
rstudent.uRegress <- function(object) {
  
  residuals.uRegress(object, type = "studentized")
  
}

#' @export
rstandard.uRegress <- function(object) {
  
  residuals.uRegress(object, type = "standardized")
  
}

#' @export
dfbeta.uRegress <- function(object, ...) {
  
  stats::dfbeta(object$fit, ...)
  
}

#' @export
dfbetas.uRegress <- function(object, ...) {
  
  stats::dfbetas(object$fit, ...)
  
}

#' @export
cooks.distance.uRegress <- function(object, ...) {
  
  stats::cooks.distance(object$fit, ...)
  
}

#' @export
hatvalues.uRegress <- function(object, ...) {
  
  stats::hatvalues(object$fit, ...)
  
}


