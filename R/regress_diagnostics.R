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

#' Extract Studentized residuals from \code{uRegress} objects
#' 
#' Extracts Studentized residuals from \code{uRegress} objects by relying on
#' functionality from the \code{stats} package.
#' 
#' @param object an object of class \code{uRegress}, as returned by
#' \link[rigr]{regress}.
#' 
#' @return a vector of Studentized residuals
#' 
#' @export
rstudent.uRegress <- function(object) {
  
  residuals.uRegress(object, type = "studentized")
  
}

#' Extract standardized residuals from \code{uRegress} objects
#' 
#' Extracts standardized residuals from \code{uRegress} objects by relying on
#' functionality from the \code{stats} package.
#' 
#' @param object an object of class \code{uRegress}, as returned by
#' \link[rigr]{regress}.
#' 
#' @return a vector of standardized residuals
#' 
#' @export
rstandard.uRegress <- function(object) {
  
  residuals.uRegress(object, type = "standardized")
  
}

#' Calculate dfbeta from \code{uRegress} objects
#' 
#' Extracts dfbeta from \code{uRegress} objects by relying on
#' functionality from the \code{stats} package. Note that 
#' \code{dfbeta} and \code{dfbetas} are not the same (\code{dfbetas} are 
#' less than the \code{dfbeta} values by a 
#' scaling factor that reflects both the leverage of the observation in
#'  question and the residual model error). 
#' 
#' @param object an object of class \code{uRegress}, as returned by
#' \link[rigr]{regress}.
#' 
#' @return a matrix of dfbeta values, with a row for each observation and a column for each model coefficient
#' 
#' @export
dfbeta.uRegress <- function(object, ...) {
  
  stats::dfbeta(object$fit, ...)
  
}

#' Calculate dfbetas from \code{uRegress} objects
#' 
#' Extracts dfbetas from \code{uRegress} objects by relying on
#' functionality from the \code{stats} package. Note that 
#' \code{dfbeta} and \code{dfbetas} are not the same (\code{dfbetas} are 
#' less than the \code{dfbeta} values by a 
#' scaling factor that reflects both the leverage of the observation in
#'  question and the residual model error). 
#' 
#' @param object an object of class \code{uRegress}, as returned by
#' \link[rigr]{regress}.
#' 
#' @return a matrix of dfbetas values, with a row for each observation and a column for each model coefficient
#' 
#' @export
dfbetas.uRegress <- function(object, ...) {
  
  stats::dfbetas(object$fit, ...)
  
}

#' Calculate Cook's distances from \code{uRegress} objects
#' 
#' Extracts Cook's distances from \code{uRegress} objects by relying on
#' functionality from the \code{stats} package. 
#' 
#' @param object an object of class \code{uRegress}, as returned by
#' \link[rigr]{regress}.
#' 
#' @return a vector of Cook's distances 
#' 
#' @export
cooks.distance.uRegress <- function(object, ...) {
  
  stats::cooks.distance(object$fit, ...)
  
}

#' Calculate the hat-values (leverages) from \code{uRegress} objects
#' 
#' Extracts hat-values (leverages) from \code{uRegress} objects by relying on
#' functionality from the \code{stats} package. 
#' 
#' @param object an object of class \code{uRegress}, as returned by
#' \link[rigr]{regress}.
#' 
#' @return a vector of hat-values (leverages)
#' 
#' @export
hatvalues.uRegress <- function(object, ...) {
  
  stats::hatvalues(object$fit, ...)
  
}


