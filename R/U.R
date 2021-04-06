## Function to perform transformation on the data
## Args: x - the variable to transform
## type - the type of transformation: "log", "dummy", "lspline", "polynomial", must be unique (i.e. "l" doesn't work)
## subset - how to subset the variable
## knots - knots for lspline
## degree - degree for polynomial
## reference - for dummy
## lbl - label, for lspline
## center - center, for polynomial
## includeAll - for dummy
## parameterization - for lspline/lsplineD
## version - version of the function
##
## Returns: a transformed x variable according to transformation in "type"
## Version: 2015 04 20
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