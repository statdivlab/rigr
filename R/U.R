#' Create a Transformed Variable
#' 
#' Creates a transformed variable using either the natural log, a dummy
#' transformation, linear splines, or a polynomial. Mostly for use in
#' regression. If a partial formula of the form \code{~var1 + var2} is entered,
#' returns the formula for use in regression. The partial formula can be named
#' by adding an equals sign before the tilde.
#' 
#' 
#' @param ...  variable(s) used to create the
#' transformation.
#' @param type a character string describing the transformation. Partial
#' matching is used, so only enough of the string to make the transformation
#' unique is needed.
#' @param subset used in creating dummy variables. Only used if \code{type ==
#' "dummy"}.
#' @param knots %% ~~Describe \code{cluster} here~~ vector of knots to create
#' the splines. Only used if \code{type=="lspline"}.
#' @param degree the degree of the polynomial to be returned. Only used if
#' \code{type=="polynomial"}.
#' @param reference the reference vector for levels of the dummy variable. Only
#' used if \code{type=="dummy"}.
#' @param lbl a label for the splines. Only used if \code{type=="lspline"}
#' @param center the center of the returned polynomial. Only used if
#' \code{type=="polynomial"}.
#' @param includeAll a logical value to use all values even in the presense of
#' a subset. Only used if \code{type=="dummy"}.
#' @param parameterization defaults to\code{"absolute"}, and provides splines
#' based on the absolute slope between knots. If \code{"change"}, provides
#' splines based on the change from knot to knot. If \code{lsplineD} is called,
#' \code{"change"} is entered by default. Only used if \code{type=="lspline"}.
#' @param vrsn if \code{TRUE}, returns the version of the function and nothing
#' else.
#' @return A matrix or vector containing the
#' transformations. The class of the returned value is
#' \code{c("transformation", y)} where \code{y} is the class of the transformed
#' variable (usually \code{numeric}). The type of transformation performed is
#' encoded as one of the attributes of the returned value, along with the
#' original data. 
#' @seealso \code{\link[uwIntroStats]{regress}}
#' @examples
#' 
#' # Reading in a dataset
#' mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt", header=TRUE)
#' attach(mri)
#' # Create a spline based on absolute
#' U(ldl, type="lspline", knots=c(70, 100, 130, 160))
#' U(ldl, type="ls", knots=c(70,100,130,160))
#' 
#' # Create a spline based on change
#' U(ldl, type="ls", knots=c(70, 100, 130, 160), parameterization="change")
#' 
#' # Create a log transformed variable
#' U(age, type="log")
#' 
#' ## Create a partial formula
#' U(ma=~male+age)
#' 
#' 
#' @export U
U <- function(..., type=NULL, subset=rep(T,length(x)), knots=NULL, degree=2, reference=sort(unique(x[!is.na(x)])), 
                           lbl=NULL, center=mean(x,na.rm=T), includeAll=FALSE, parameterization="absolute", vrsn=FALSE){
  
  L <- list(...)
  hypernames <- names(unlist(match.call(expand.dots=F)$...))
  names(L) <- unlist(match.call(expand.dots=F)$...)
  if(!is.null(hypernames)){
    names(L) <- hypernames
  }
  if(length(L)==1){
    x <- unlist(L)
  } else {
    x <- as.data.frame(L)
    dimnames(x)[[2]] <- names(L)
  }
  version <- "20150420"
  if(vrsn){
    return(version)
  }
  findx <- pmatch(type, c("log", "dummy", "lspline", "polynomial"))
  if(is.null(type)){
    return(L)
  }
  if(is.na(findx)){
    stop("Unsupported type or multiple matches with acceptable types")
  }
  type <- c("log", "dummy", "lspline", "polynomial")[findx]
  if(length(dim(x)[2])==0){
    if(type=="log"){
      if(!is.null(knots)){
        warning("Knots will not be used in log transformation")
      }
      X <- log(x)
    } else if (type=="dummy"){
      X <- dummy(x, subset=subset, reference=reference, includeAll=includeAll)
    } else if (type=="lspline"){
      X <- lspline(x, knots=knots, lbl=lbl, parameterization=parameterization)
    } else if (type=="polynomial"){
      X <- polynomial(x, degree=degree, center=center)
    } else {
      if(!is.null(type)){
        stop("Unsupported type")
      }
    }
  } else {
    if(type=="log"){
      if(!is.null(knots)){
        warning("Knots will not be used in log transformation")
      }
      X <- apply(x, 2, log)
    } else if (type=="dummy"){
      X <- apply(x, 2, dummy, subset=subset, reference=reference, includeAll=includeAll)
    } else if (type=="lspline"){
      X <- apply(x, 2, lspline, knots=knots, lbl=lbl, parameterization=parameterization)
    } else if (type=="polynomial"){
      X <- apply(x, 2, polynomial, degree=degree, center=center)
    } else {
      if(!is.null(type)){
        stop("Unsupported type")
      }
    }
  }
  tmp <- class(X)
  attr(X,"transformation") <- type
  attr(X, "reference") <- reference
  attr(X, "name") <- names(L)
  attr(X, "original") <- x
  class(X) <- c("transformation", tmp)
  return(X)
}
