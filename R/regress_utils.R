#' Fully parse a formula for regress()
#' Takes in formula, potentially with nested F-tests
#' 
#' @param form the formula to parse
#' @param modelframe the model frame which will become the model matrix
#' @param mat the matrix of logicals to run on
#' @param data the data frame
#' 
#' @return a list of lists of tests to perform (and overall formula)
#' 
#' @keywords internal
#' @noRd
testList <- function(form, modelframe, mat, data){
  tmplist <- parsePartials(form[[3]], modelframe, mat)
  charForm <- parseParseFormula(parseFormula(form[[3]], modelframe, mat))
  charForm <- paste(deparse(form[[2]]), deparse(form[[1]]),paste(charForm, collapse=""), sep="")
  tmplist2 <- LinearizeNestedList(tmplist)
  tmplist <- parseList(tmplist2)
  forms <- getTerms(form, form[[2]])
  forms <- parseList(LinearizeNestedList(forms))
  names(forms) <- c("overall", names(tmplist))
  if(length(tmplist)!=0){
    form <- as.formula(charForm, env=.GlobalEnv)
  }
  return(list(formula=form, testList=tmplist, termList = forms))
}

#' Function which takes in a string and returns a vector of individual characters
#' Helper function for print.uRegress
#' 
#' @param str a string
#' 
#' @return a vector of individual characters
#' 
#' @keywords internal
#' @noRd
explode <- function(str){
  chars <- strsplit(str, "")[[1]]
  return (chars)
}

#' Takes a formula, returns a list of terms
#' 
#' @param form the formula
#' @param y
#' 
#' @return the list of terms
#' 
#' @keywords internal
#' @noRd
getTerms <- function(form, y){
  f1 <- as.list(form)
  if(length(f1)==2){
    if(storage.mode(form)=="symbol"){
      return(NULL)
    } else{
      chars <- deparse(form)
      if((grepl("U", f1[[1]]))){
        if(grepl("=", chars, fixed=TRUE)){
          chars <- unlist(strsplit(chars, "=", fixed=TRUE))
          if(length(chars)>2){
            chars <- c(chars[1], paste(chars[-1], collapse="="))
          }
        } else {
          chars <- unlist(strsplit(chars, "(", fixed=TRUE))
        }
        chars[length(chars)] <- paste(unlist(strsplit(chars[length(chars)], ")", fixed=TRUE)), collapse=")")
        chars <- paste(chars[-1], collapse="(")
        chars <- paste(deparse(y), chars, sep="")
        form <- as.formula(chars, env=.GlobalEnv)
        return(list(terms(form), getTerms(f1[[2]], y)))
      }
      return(getTerms(f1[[2]], y))
    }
  } else if (length(f1)==1){
    if(storage.mode(form)=="symbol"){
      return(NULL)
    } else{
      return(NULL)
    }
  } else {
    if(grepl("~", form[[1]], fixed=TRUE)){
      ret <- terms(form)
    } else {
      ret <- NULL
    }
    return(list(ret, lapply(f1, getTerms, form[[2]])))
  }
}

#' Takes a formula, returns a viable formula
#' 
#' @param form the formula
#' @param modelframe
#' @param mat
#' 
#' @return the list 
#' 
#' @keywords internal
#' @noRd
parseFormula <- function(form, modelframe, mat){
  f1 <- as.list(form)
  if(length(f1)==1){
    return(form)
  } else if(length(f1)==2) {
    if(sum(grepl("U", f1)) > 1){
      ## if more than one U, need to evaluate nested U and then evaluate overall U
      tmplistOne <- parsePartials(f1[[2]][[2]], modelframe, mat)
      indx <- which(grepl("U", f1[[2]][[2]]))
      for(i in indx){
        char <- deparse(f1[[2]][[2]][[i]])
        char2 <- unlist(strsplit(char, "~", fixed=TRUE))
        if(sum(grepl("U", char2))>1){
          char2 <- unlist(strsplit(char2, "U"))
          ## add on the appropriate parentheses
          char2 <- unlist(strsplit(char2, "U"))
          ## take everything with no equals sign
          char2 <- char2[!grepl("=", char2, fixed=TRUE)]
          char2 <- paste(char2, collapse="(")
        } else {
          char2 <- char2[!grepl("U", char2)]
        }
        char2 <- paste("(", char2, sep="")
        f1tmp <- as.formula(paste("~", char2))
        f1[[2]][[2]][[i]] <- f1tmp[[2]]
      }
      form[[2]] <- f1[[2]]
    } 
    if(sum(grepl("U", f1))==1){
      ## try to evaluate the formula
      tmp <- tryCatch({eval(form, parent.frame())}, error=function(e){NULL})
      if(!is.null(tmp)){
        tmpForList <- modelframe
        tmpForList <-  tmpForList[c(1L, mat)]
        tmpForList$drop.unused.levels <- TRUE
        nms <- names(tmp)
        tmp <- tmp[[1]]
        tmpForList$formula <- tmp
        tmpForList[[1L]] <- quote(stats::model.frame)
        tmpForList <- eval(tmpForList, parent.frame())
        tmplist <- list(tmpForList)
        if(grepl("~", nms)){
          names(tmplist) <- paste("U(",as.character(tmp)[2],")",sep="")
        } else {
          names(tmplist) <- nms
        }
        formNew <- f1[[2]][[2]]
        return(formNew)
      } else {
        return(form)
      }
    } else {
      return(form)
    }
  } else {
    return(lapply(form, parseFormula, modelframe, mat))
  }
}

#' Go through a parseFactor and get the relevant formulas and partials
#' 
#' @param lst a list returned by parseFactor
#' 
#' @return a list with two components - the formula and a list of all of the matrices
#' 
#' @keywords internal
#' @noRd
parseParseFormula <- function(lst){
  tmp <- unlist(lst)
  parsetmp <- sapply(tmp, deparse)
  len <- floor(length(parsetmp)/2)
  oper <- rev(parsetmp[1:len])
  
  vec <- matrix(parsetmp[(len+1):length(parsetmp)], nrow=1)
  ret <- insertVec(vec, oper, 2)
  return(ret)
}

#' Inserts a vector into another vector
#' 
#' @param x1
#' @param x2
#' @param indx
#' 
#' @return 
#' 
#' @keywords internal
#' @noRd
insertVec <- function(x1, x2, indx){
  if(length(x2)==1){
    if(length(x1)==1){
      return(c(x1,x2))
    } else {
      return(c(x1[1:indx-1], x2, x1[indx:length(x1)]))
    }
  } else {
    tmp1 <- insertVec(x1, x2[1], indx)
    tmp2 <- insertVec(tmp1, x2[-1], indx+2)
    return(tmp2)
  }
}

#' Taken from https://sites.google.com/site/akhilsbehl/geekspace/articles/r/linearize_nested_lists_in_r
#' Author: Akhil S Bhel
#' 
#' Implements a recursive algorithm to linearize nested lists upto any
#' arbitrary level of nesting (limited by R's allowance for recursion-depth).
#' By linearization, it is meant to bring all list branches emanating from
#' any nth-nested trunk upto the top-level trunk s.t. the return value is a
#' simple non-nested list having all branches emanating from this top-level
#' branch.
#' 
#' Since dataframes are essentially lists a boolean option is provided to
#' switch on/off the linearization of dataframes. This has been found
#' desirable in the author's experience.
#' 
#' Also, one'd typically want to preserve names in the lists in a way as to
#' clearly denote the association of any list element to it's nth-level
#' history. As such we provide a clean and simple method of preserving names
#' information of list elements. The names at any level of nesting are
#' appended to the names of all preceding trunks using the `NameSep` option
#' string as the seperator. The default `/` has been chosen to mimic the unix
#' tradition of filesystem hierarchies. The default behavior works with
#' existing names at any n-th level trunk, if found; otherwise, coerces simple
#' numeric names corresponding to the position of a list element on the
#' nth-trunk. Note, however, that this naming pattern does not ensure unique
#' names for all elements in the resulting list. If the nested lists had
#' non-unique names in a trunk the same would be reflected in the final list.
#' Also, note that the function does not at all handle cases where `some`
#' names are missing and some are not.
#' 
#' Clearly, preserving the n-level hierarchy of branches in the element names
#' may lead to names that are too long. Often, only the depth of a list
#' element may only be important. To deal with this possibility a boolean
#' option called `ForceNames` has been provided. ForceNames shall drop all
#' original names in the lists and coerce simple numeric names which simply
#' indicate the position of an element at the nth-level trunk as well as all
#' preceding trunk numbers.
#' 
#' @param NList
#' @param LinearizeDataFrames
#' @param NameSep
#' @param ForceNames
#' 
#' @return LinearList: Named list
#' 
#' @keywords internal
#' @noRd
LinearizeNestedList <- function(NList, LinearizeDataFrames=FALSE,
                                NameSep="/", ForceNames=FALSE) {
  
  # Sanity checks:
  #
  stopifnot(is.character(NameSep), length(NameSep) == 1)
  stopifnot(is.logical(LinearizeDataFrames), length(LinearizeDataFrames) == 1)
  stopifnot(is.logical(ForceNames), length(ForceNames) == 1)
  if (! is.list(NList)) return(NList)
  #
  # If no names on the top-level list coerce names. Recursion shall handle
  # naming at all levels.
  #
  if (is.null(names(NList)) | ForceNames == TRUE)
    names(NList) <- as.character(1:length(NList))
  #
  # If simply a dataframe deal promptly.
  #
  if (is.data.frame(NList) & LinearizeDataFrames == FALSE)
    return(NList)
  if (is.data.frame(NList) & LinearizeDataFrames == TRUE)
    return(as.list(NList))
  #
  # Book-keeping code to employ a while loop.
  #
  A <- 1
  B <- length(NList)
  #
  # We use a while loop to deal with the fact that the length of the nested
  # list grows dynamically in the process of linearization.
  #
  while (A <= B) {
    Element <- NList[[A]]
    EName <- names(NList)[A]
    if (is.list(Element)) {
      #
      # Before and After to keep track of the status of the top-level trunk
      # below and above the current element.
      #
      if (A == 1) {
        Before <- NULL
      } else {
        Before <- NList[1:(A - 1)]
      }
      if (A == B) {
        After <- NULL
      } else {
        After <- NList[(A + 1):B]
      }
      #
      # Treat dataframes specially.
      #
      if (is.data.frame(Element)) {
        if (LinearizeDataFrames == TRUE) {
          #
          # `Jump` takes care of how much the list shall grow in this step.
          #
          Jump <- length(Element)
          NList[[A]] <- NULL
          #
          # Generate or coerce names as need be.
          #
          if (is.null(names(Element)) | ForceNames == TRUE)
            names(Element) <- as.character(1:length(Element))
          #
          # Just throw back as list since dataframes have no nesting.
          #
          Element <- as.list(Element)
          #
          # Update names
          #
          names(Element) <- paste(EName, names(Element), sep=NameSep)
          #
          # Plug the branch back into the top-level trunk.
          #
          NList <- c(Before, Element, After)
        }
        Jump <- 1
      } else {
        NList[[A]] <- NULL
        #
        # Go recursive! :)
        #
        if (is.null(names(Element)) | ForceNames == TRUE)
          names(Element) <- as.character(1:length(Element))
        Element <- LinearizeNestedList(Element, LinearizeDataFrames,
                                       NameSep, ForceNames)
        names(Element) <- paste(EName, names(Element), sep=NameSep)
        Jump <- length(Element)
        NList <- c(Before, Element, After)
      }
    } else {
      Jump <- 1
    }
    #
    # Update book-keeping variables.
    #
    A <- A + Jump
    B <- length(NList)
  }
  return(NList)
}

#' Takes in a linearized list, returns a list with only the non-null components
#' 
#' @param lst
#' 
#' @return a list with only the non-null components
#' 
#' @keywords internal
#' @noRd
parseList <- function(lst){
  indx <- !sapply(lst, is.null)
  tmp <- lst[indx]
  nms <- names(tmp)
  nms <- sapply(nms, explode, simplify="array")
  
  if(any(indx)){
    #if nms is a list
    if(is.list(nms)){
      newnms <- lapply(nms, getindx)
      indx2 <- lapply(newnms, which)
      if(!any(sapply(newnms, any))){
        len <- length(tmp)
        nmtmp <- unlist(strsplit(names(tmp), "/"))
        indx3 <- suppressWarnings(as.numeric(nmtmp))
        if(any(is.na(indx3))){
          for(i in 1:len){
            nms[[i]] <- nmtmp[which(is.na(indx3))[i]]
          }
        } else {
          for(i in 1:len){
            nms[[i]] <- nmtmp[i+1]
          }
        }
      } else {
        for(i in 1:length(nms)){
          nms[[i]] <- getnm(matrix(nms[[i]], ncol=1), indx2[[i]])
        }
      }
    } else{
      if(is.matrix(nms)){
        newnms <- apply(nms, 2, getindx)
        indx2 <- apply(newnms, 2, which)
      } else {
        newnms <- getindx(nms)
        indx2 <- which(newnms)
      }
      if (length(indx2)==0){
        len <- length(tmp)
        nmtmp <- unlist(strsplit(names(tmp), "/"))
        nms <- rep(NA, len)
        for(i in 1:len){
          nms[i] <- nmtmp[i*2]
        }
      } else if(class(indx2)=="integer"){
        nms <- getnm(nms, max(indx2))
      } else if(dim(indx2)[2]==1) {
        nms <- getnm(nms, max(indx2))
      } else {
        nms <- getnm(nms, indx2)
      }
    }
    names(tmp) <- nms
  }
  return(tmp)
}

#' Used in parseList
#' @keywords internal
#' @noRd
getindx <- function(strlst){
  return(sapply(strlst, function(x) x=="U"))
}

#' Used in parseList
#' @keywords internal
#' @noRd
getnm <- function(strmat, indx){
  newmat <- paste(strmat[indx[1]:dim(strmat)[1],1], collapse="")
  if(length(indx)>1){
    for(i in 2:length(indx)){
      newmat <- c(newmat, paste(strmat[indx[i]:dim(strmat)[1],i], collapse=""))
    }
  }
  return(newmat)
}

#' Takes a formula, returns a list of models for multiple partial tests
#' 
#' @param form the formula
#' @param modelframe
#' @param mat
#' 
#' @return the list
#' 
#' @keywords internal
#' @noRd
parsePartials <- function(form, modelframe, mat){
  f1 <- as.list(form)
  if(length(f1)==1){
    return(NULL)
  } else if(length(f1)==2) {
    tmplistOne <- NULL
    if(sum(grepl("U", f1)) > 1){
      ## if more than one U, need to evaluate nested U and then evaluate overall U
      tmplistOne <- parsePartials(f1[[2]][[2]], modelframe, mat)
      tmp <- LinearizeNestedList(tmplistOne)
      tmplistOne <- parseList(tmp)
      indx <- which(grepl("U", f1[[2]][[2]]))
      for(i in indx){
        char <- deparse(f1[[2]][[2]][[i]])
        char2 <- unlist(strsplit(char, "~", fixed=TRUE))
        if(sum(grepl("U", char2))>1){
          char2 <- unlist(strsplit(char2, "U"))
          ## add on the appropriate parentheses
          char2 <- unlist(strsplit(char2, "U"))
          ## take everything with no equals sign
          char2 <- char2[!grepl("=", char2, fixed=TRUE)]
          char2 <- paste(char2, collapse="(")
        } else {
          char2 <- char2[!grepl("U", char2)]
        }
        char2 <- paste("(", char2, sep="")
        f1tmp <- as.formula(paste("~", char2))
        f1[[2]][[2]][[i]] <- f1tmp[[2]]
      }
      form[[2]] <- f1[[2]]
    } 
    if(sum(grepl("U", f1))==1){
      ## try to evaluate the formula
      tmp <- tryCatch({eval(form, parent.frame())}, error=function(e){NULL})
      if(!is.null(tmp)){
        tmpForList <- modelframe
        tmpForList <-  tmpForList[c(1L, mat)]
        tmpForList$drop.unused.levels <- TRUE
        nms <- names(tmp)
        if(!is.null(tmplistOne)){
          nms <- c(nms, names(tmplistOne))
        }
        tmp <- tmp[[1]]
        tmpForList$formula <- tmp
        tmpForList[[1L]] <- quote(stats::model.frame)
        tmpForList <- eval(tmpForList, parent.frame())
        tmplist <- list(tmpForList)
        if(!is.null(tmplistOne)){
          tmplist <- c(tmplist, tmplistOne)
        }
        if(any(grepl("~", nms))){
          names(tmplist) <- paste("U(",as.character(tmp)[2],")",sep="")
        } else {
          names(tmplist) <- nms
        }
        return(tmplist)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  } else {
    return(lapply(form, parsePartials, modelframe, mat))
  }
}

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

#' A function to traverse the termlist tree,
#' and create the correct augCoefficients matrix.
#' Helper function for regress()
#' 
#' @param termlist the list with terms
#' @param mat the matrix to correct
#' @param ind
#' 
#' @return A list with the updated augmented coefficients matrix and the current indices
#' 
#' @keywords internal
#' @noRd
termTraverse <- function(termlist, mat, ind){
  current <- termlist
  lbls <- attr(current[[1]], "term.labels")
  hasU <- grepl("U", lbls)
  if(any(hasU)){
    if(any(names(current)=="overall")){
      return(termTraverse(termlist[-1], mat, ind))
    } else {
      tmp <- reFormat(termlist[1], lbls, mat, ind)
      mat <- tmp$mat
      #       ind <- tmp$ind
      return(termTraverse(termlist[-1], mat, ind))
    }
  } else if (length(termlist)>1){
    t1 <- termTraverse(termlist[1], mat, ind)
    m1 <- t1$mat
    i1 <- t1$ind
    return(termTraverse(termlist[-1], m1, i1))
  } else {
    #reformat goes here if needed
    tmp <- reFormat(current, lbls, mat, ind)
    mat <- tmp$mat
    curIndx <- tmp$ind
    return(list(mat=mat, ind=curIndx))
  }
}

#' A function to check if all values in a vector are equal
#' 
#' @param x a vector
#' 
#' @return \code{TRUE}/\code{FALSE}
#' 
#' @keywords internal
#' @noRd
equal <- function(x){
  if(length(x)==1){
    return(TRUE)
  } else {
    if(x[1]!=x[2]){
      return(FALSE)
    } else {
      return(equal(x[-1]))
    }
  }
}

#' A helper function for termTraverse
#' 
#' @param current the current terms
#' @param lbls the labels of the terms
#' @param mat the augmented coefficients matrix
#' @param ind the current indices
#' 
#' @return A list with an updated augmented coefficients matrix and the current indices
#' 
#' @keywords internal
#' @noRd
reFormat <- function(current, lbls, mat, ind){
  include <- grepl(names(current), dimnames(mat)[[1]], fixed=TRUE)
  includeHasInter <- grepl(":", dimnames(mat[include,])[[1]])
  include[include][includeHasInter] <- FALSE
  hasU <- grepl("U", lbls)
  if(any(hasU)){
    tmp <- lbls[hasU]
    ## only split if there is a name to the U
    hasEq <- grepl("=", tmp, fixed=TRUE)
    tmp2 <- tmp[hasEq]
    
    tmp2 <- unlist(strsplit(tmp2, "U", fixed=TRUE))
    if(sum(grepl(")", tmp2, fixed=TRUE))!=length(tmp2)-1){
      tmp2 <- tmp2[-which(grepl(")", tmp2, fixed=TRUE))]
    }
    tmp2 <- as.matrix(tmp2)
    tmp2 <- apply(tmp2, 1, splitOnParen)
    hasEq2 <- grepl("=", tmp2, fixed=TRUE)
    tmp2[hasEq2] <- unlist(lapply(strsplit(tmp2[hasEq2], "=", fixed=TRUE), function(x) return(x[1])))
    
    tmp[hasEq] <- pasteTwo(tmp2)
    lbls[hasU] <- tmp
    lbls <- unlist(lapply(strsplit(lbls, " ", fixed=TRUE), function(x) return(x[1])))
  }
  rows <- sapply(lbls, grepl, dimnames(mat)[[1]], fixed=TRUE)
  nestRows <- sapply(" ", grepl, dimnames(mat)[[1]], fixed=TRUE)
  parenRows <- sapply("(", grepl, dimnames(mat)[[1]], fixed=TRUE)
  nestRows[nestRows&parenRows] <- FALSE
  ## check if it is an interaction with the wrong thing
  interRows <- sapply(":", grepl, dimnames(mat)[[1]], fixed=TRUE)
  
  if(is.matrix(rows[interRows,])){
    rows[interRows,] <- t(apply(rows[interRows,], 1, function(x) if(!equal(x)) return(rep(FALSE, length(x))) else return(x)))
  }
  ## look at each column of rows. if nested are immediately after 
  ## a row, put the nested in
  rows <- apply(rows, 2, checkNesting, nestRows)
  rows <- apply(rows, 1, sum)
  rows <- ifelse(rows>=1, TRUE, FALSE)
  indices <- which(rows)
  indx <- max(which(include))
  curIndx <- 1:length(ind)
  bool <- !any(ind > curIndx)
  if(bool){
    dex <- NULL
  } else {
    dex <- min(which(ind>curIndx))
    if(dex-1==1 & length(which(ind>curIndx))>1){
      dex <- min(which(ind>curIndx)[-1])
    }
  }
  if(bool){
    if(min(indices)<indx){
      curIndx <- c(1:(min(indices)-1), indx, indices, which(!rows))
    } else {
      curIndx <- c(1:indx, indices, which(!rows))
    }
  } else if (!any(interRows)){
    if(max(indices)-min(indices)==1){
      curIndx <- c(1:(min(indices[-1])-2), indx, indices, which(!rows))
    } else {
      curIndx <- c(1:(min(indices[-1])-1), indx, indices, which(!rows))
    }
  } else {
    curIndx <- c(1:dex, indx, indices, which(!rows))
  }
  len <- max(which(curIndx==1))-1
  tmpIndx <- curIndx[1:len]
  if(length(tmpIndx)<length(ind)){
    curIndx <- unique(curIndx)
  } else if (tmpIndx[length(tmpIndx)]==ind[length(ind)]){
    if(length(tmpIndx)-1 >= length(ind) & tmpIndx[length(tmpIndx)-1]!=ind[length(ind)-1]){
      curIndx <- tmpIndx[-length(tmpIndx)]
    } else {
      curIndx <- unique(curIndx)
    }
  } else {
    curIndx <- tmpIndx
  }
  
  mat <- mat[curIndx,]
  return(list(mat=mat, ind=curIndx))
}

#' Function to paste every two elements in a vector together
#' Helper for termTraverse, regress
#' 
#' @param vec the vector to paste together
#' 
#' @return a vector with every two elements pasted together
#' 
#' @keywords internal
#' @noRd
pasteTwo <- function(vec){
  if(length(vec)==2){
    return(paste(vec[1], vec[2], sep=""))
  } else {
    one <- pasteTwo(vec[1:2])
    two <- pasteTwo(vec[-c(1:2)])
    return(c(one, two))
  }
}

#' Function to add the correct args onto a polynomial, dummy, or lspline term
#' Used to live in pasting_args.R
#' 
#' @param varname
#' @param var
#' @param type
#' 
#' @return 
#' 
#' @keywords internal
#' @noRd
addArgs <- function(varname, var, type){
  findx <- pmatch(type, c("polynomial", "lspline", "dummy"))
  type <- c("polynomial", "lspline", "dummy")[findx]
  
  att <- attributes(var)
  args <- att$transformation
  if(type=="polynomial"){
    args <- list(transformation=args, degree=max(att$degree), center=att$center, nm=att$prnm, param=att$transformation)
    varname <- unlist(strsplit(varname, "(", fixed=TRUE))[2]
    varname <- unlist(strsplit(varname, ")", fixed=TRUE))[1]
    varname <- unlist(strsplit(varname, ",", fixed=TRUE))[1]
    varnames <- rev(pasteOn(rep(varname, args$degree), "^", args$degree))
  } else if(type=="dummy"){
    args <-list(transformation=args, reference=min(att$reference), num=length(att$reference)-1, nm=att$prnm, param=att$transformation)
    varname <- unlist(strsplit(varname, "(", fixed=TRUE))[2]
    varname <- unlist(strsplit(varname, ")", fixed=TRUE))[1]
    varname <- unlist(strsplit(varname, ",", fixed=TRUE))[1]
    varnames <- rev(pasteOn(rep(varname, att$dim[2]), ".", att$dim[2]+args$reference))
  } else {
    varname <- unlist(strsplit(varname, "(", fixed=TRUE))[2]
    mini <- any(grepl("min", att$dimnames[[2]], fixed=TRUE))
    if(mini){
      att$knots <- c("min", att$knots)
    }
    args <- list(transformation=args, knots=att$knots, num=length(att$knots), nm=att$prnm, param=att$param)
    varname <- unlist(strsplit(varname, ")", fixed=TRUE))[1]
    varname <- unlist(strsplit(varname, ",", fixed=TRUE))[1]
    
    varnames <- pasteOnSpline(rep(varname, length(att$knots)), att$knots)
  }
  return(list(varnames, args))
}

#' Function to paste on degree
#' Used to live in pasting_args.R
#' 
#' @param x
#' @param str
#' @param num
#' 
#' @return 
#' 
#' @keywords internal
#' @noRd
pasteOn <- function(x, str, num){
  if(length(x)==1){
    return(paste(x, str, num, sep=""))
  } else{
    ret <- paste(x[1], str, num, sep="")
    ret2 <- pasteOn(x[-1], str, num-1)
    return(c(ret, ret2))
  }
}

#' Helper function for lspline
#' Used to live in pasting_args.R
#' 
#' @param x
#' @param nums
#' 
#' @return 
#' 
#' @keywords internal
#' @noRd
pasteOnSpline <- function(x, nums){
  if(length(x)==1){
    return(paste(x, ".K(", nums, ")", sep=""))
  } else{
    ret <- paste(x[1], ".K(", nums[1], ")", sep="")
    ret2 <- pasteOnSpline(x[-1], nums[-1])
    return(c(ret, ret2))
  }
}

#' Function that takes a pair and pastes together, with colon
#' Used to live in pasting_args.R
#' 
#' @param vec
#' 
#' @return 
#' 
#' @keywords internal
#' @noRd
pastePair <- function(vec){
  if(length(vec)==2){
    return(paste(vec[1], ":", vec[2], sep=""))
  } else{
    ret1 <- paste(vec[1], ":", vec[2], sep="")
    ret2 <- pastePair(vec[-c(1,2)])
    return(c(ret1, ret2))
  }
}

#' Function that sums across a logical vector, returns the current value at each point
#' Used to live in pasting_args.R
#' 
#' @param vec
#' 
#' @return 
#' 
#' @keywords internal
#' @noRd
movingSum <- function(vec){
  s <- rep(0, length(vec))
  for(i in 1:length(vec)){
    if(vec[i]){
      s[i] <- i
    } else {
      s[i] <- 0
    }
  }
  return(s)
}

#' Used to live in pasting_args.R
#' 
#' @param num
#' @param vec
#' 
#' @return 
#' 
#' @keywords internal
#' @noRd
myNext <- function(num, vec){
  if(which(vec==num)+1<= length(vec)){
    ret <- vec[which(vec==num)+1]
  } else {
    ret <- which(vec==num)+1
  }
  return(ret)
}

#' Function to tack on args and return appropriate names for printing
#' Used to live in pasting_args.R
#' 
#' @param p
#' @param h
#' @param mf
#' 
#' @return 
#' 
#' @keywords internal
#' @noRd
reFormatReg <- function(p, h, mf){
  lspline <- grepl("lspline", h)
  polynomial <- grepl("polynomial", h)
  dummy <- grepl("dummy", h)
  args <- as.list(h)
  
  parens <- grepl(")", p, fixed=TRUE)
  interact <- grepl(":", p, fixed=TRUE)
  indices <- lspline | dummy | polynomial
  indices <- movingSum(indices)
  indices <- indices[indices>0]
  indx <- indices[1]
  #cols <- preds ## fix cols in loops
  if(any(lspline)){
    tmp <- h[lspline]
    for(i in 1:length(tmp)){
      tmp2 <- addArgs(tmp[i], mf[,tmp[i]], type="ls")
      args[[indx]] <- tmp2[[2]]
      nms <- tmp2[[1]]
      ## get the correct naming
      current <- p[grepl(tmp[i], p, fixed=TRUE)]
      split <- unlist(strsplit(current, ":", fixed=TRUE))
      ## remove the extraneous "min", 85, etc
      if(any(split=="min")){
        split <- split[split!="min"]
      }
      if(any(split=="max")){
        split <- split[split!="max"]
      }
      args[[indx]]$num <- args[[indx]]$num
      numSplit <- suppressWarnings(as.numeric(split))
      if(any(!is.na(numSplit))){
        split <- split[is.na(numSplit)]
      }
      bool <- length(current)==length(split)
      repsplit <- split[grepl(tmp[i], split, fixed=TRUE)]
      even <- FALSE
      if(length(grepl(tmp[i], split, fixed=TRUE))>=3){
        if(grepl(tmp[i], split, fixed=TRUE)[3]){
          even <- FALSE
        } else {
          even <- TRUE
        }
      }
      if(!even){
        repsplit <- rep(nms, length(repsplit)/(args[[indx]]$num))
      } else {
        len <- length(repsplit)/(args[[indx]]$num)-1
        repsplit <- rep(nms, 1)
        for(j in 1:length(nms)){
          repsplit <- c(repsplit, rep(nms[j], len))
        }
      }
      split[grepl(tmp[i], split, fixed=TRUE)] <- repsplit
      ## paste back in, if interactions
      if(!bool){
        split[(args[[indx]]$num+1):length(split)] <- pastePair(split[(args[[indx]]$num+1):length(split)])
        split <- unique(split)
        current <- split
      } else {
        current <- split
      }
      p[grepl(tmp[i], p, fixed=TRUE)] <- current
      indx <- myNext(indx, indices)
    }
  }
  if(any(polynomial)){
    tmp <- h[polynomial]
    for(i in 1:length(tmp)){
      tmp2 <- addArgs(tmp[i], mf[,tmp[i]], type="poly")
      args[[indx]] <- tmp2[[2]]
      nms <- tmp2[[1]]
      ## get the correct naming
      current <- p[grepl(tmp[i], p, fixed=TRUE)]
      split <- unlist(strsplit(current, ":", fixed=TRUE))
      bool <- length(current)==length(split)
      repsplit <- split[grepl(tmp[i], split, fixed=TRUE)]
      even <- FALSE
      if(length(grepl(tmp[i], split, fixed=TRUE))>=3){
        if(grepl(tmp[i], split, fixed=TRUE)[3]){
          even <- FALSE
        } else {
          even <- TRUE
        }
      }
      if(!even){
        repsplit <- rep(nms, length(repsplit)/(args[[indx]]$degree))
      } else {
        len <- length(repsplit)/(args[[indx]]$degree)-1
        repsplit <- rep(nms, 1)
        for(j in 1:length(nms)){
          repsplit <- c(repsplit, rep(nms[j], len))
        }
      }
      split[grepl(tmp[i], split, fixed=TRUE)] <- repsplit
      ## paste back in, if interactions
      if(!bool){
        split[(args[[indx]]$degree+1):length(split)] <- pastePair(split[(args[[indx]]$degree+1):length(split)])
        split <- unique(split)
        current <- split
      } else {
        current <- split
      }
      p[grepl(tmp[i], p, fixed=TRUE)] <- current
      indx <- myNext(indx, indices)
    }
  } 
  if(any(dummy)){
    tmp <- h[dummy]
    for(i in 1:length(tmp)){
      tmp2 <- addArgs(tmp[i], mf[,tmp[i]], type="dum")
      args[[indx]] <- tmp2[[2]]
      nms <- tmp2[[1]]
      ## get the correct naming
      current <- p[grepl(tmp[i], p, fixed=TRUE)]
      split <- unlist(strsplit(current, ":", fixed=TRUE))
      bool <- length(current)==length(split)
      repsplit <- split[grepl(tmp[i], split, fixed=TRUE)]
      even <- FALSE
      if(length(grepl(tmp[i], split, fixed=TRUE))>=3){
        if(grepl(tmp[i], split, fixed=TRUE)[3]){
          even <- FALSE
        } else {
          even <- TRUE
        }
      }
      if(!even){
        repsplit <- rep(nms, length(repsplit)/(args[[indx]]$num))
      } else {
        len <- length(repsplit)/(args[[indx]]$num)-1
        repsplit <- rep(nms, 1)
        for(j in 1:length(nms)){
          repsplit <- c(repsplit, rep(nms[j], len))
        }
      }
      split[grepl(tmp[i], split, fixed=TRUE)] <- repsplit
      ## paste back in, if interactions
      if(!bool){
        split[(args[[indx]]$num+1):length(split)] <- pastePair(split[(args[[indx]]$num+1):length(split)])
        split <- unique(split)
        current <- split
      } else {
        current <- split
      }
      p[grepl(tmp[i], p, fixed=TRUE)] <- current
      indx <- myNext(indx, indices)
    }
  }
  return(list(preds=p, args=args))
}

#' Function to create the correct columns for processTerm
#' Takes a predictor and the terms vector, determines which it is,
#' if an interaction, checks both
#' don't put in the intercept
#' Used to live in pasting_args.R
#' 
#' @param pds predictor
#' @param tms terms vector
#' 
#' @return 
#' 
#' @keywords internal
#' @noRd
createCols <- function(pds, tms){
  interact <- grepl(":", pds, fixed=TRUE)
  lspline <- grepl("lspline", pds, fixed=TRUE)
  if(lspline){
    char <- explode(pds)
    if(sum(grepl(":", char, fixed=TRUE))==1){
      interact <- rep(FALSE, length(interact))
    }
  }
  if(!interact){
    ## split on )
    pds <- unlist(strsplit(pds, ")"))[1]
    t <- tms[grepl(pds, tms, fixed=TRUE)][1]
    if(sum(grepl(pds, tms, fixed=TRUE))>2 & any(grepl("lspline", tms))){
      t <- tms[grepl(pds, tms, fixed=TRUE)][2]
    }
  } else {
    vec <- unlist(strsplit(pds, ":"))
    if(lspline){
      vec <- vec[-2]
    }
    t <- NULL
    for(i in 1:length(vec)){
      pds <- unlist(strsplit(vec[i], ")"))[1]
      if(sum(grepl(pds, tms, fixed=TRUE))>2 & lspline){
        t <- c(t,tms[grepl(pds, tms, fixed=TRUE)][2])
      } else {
        t <- c(t, tms[grepl(pds, tms, fixed=TRUE)][1])
      }
    }
    t <- paste(t, collapse=":")
  }
  return(t)
}

#' Used to live in pasting_args.R
#' 
#' @param col
#' @param nested 
#' 
#' @return 
#' 
#' @keywords internal
#' @noRd
checkNesting <- function(col, nested){
  if(!any(col)){
    rowInd <- length(col)
  } else {
    rowInd <- min(which(col))
  }
  ind <- 1
  while(nested[rowInd+ind]&rowInd+ind <= length(nested)){
    col[rowInd+ind] <- TRUE
    ind <- ind+1
  }
  return(col)
}

#' Function to split on parentheses
#' Helper function for termTraverse, regress
#' Used to live in pasting_args.R
#' 
#' @param x the string
#' 
#' @return a string with no parentheses
#' 
#' @keywords internal
#' @noRd
splitOnParen <- function(x){
  char <- explode(x)
  if(length(char)>0){
    if(char[1]=="("){
      if(char[length(char)]==")"){
        return(paste(char[-c(1, length(char))], collapse=""))
      } else {
        return(paste(char[-1], collapse=""))
      }
    } else {
      return(x)
    }
  } else {
    return(x)
  }
}

#' likelihood ratio test
#' Used to live in uLRtest.R
#' 
#' @param full
#' @param reduced
#' 
#' @return 
#' 
#' @keywords internal
#' @noRd
uLRtest <- function (full, reduced) {
  
  if (dimnames(full$coefficients)[[2]][2]=="Robust SE")
    stop("uLRtest inappropriate with robust SE")
  if (dimnames(reduced$coefficients)[[2]][2]=="Robust SE") 
    stop("uLRtest inappropriate with robust SE")
  if (!all(dimnames(reduced$coefficients)[[1]] %in% dimnames(reduced$coefficients)[[1]])) 
    stop("uLRtest only appropriate with hierarchical models")
  if (dim(reduced$lmobj$model)[1] != dim(full$lmobj$model)[1]) 
    stop("full and reduced models must be based on same data")
  if (any(dimnames(reduced$lmobj$model)[[1]] != dimnames(full$lmobj$model)[[1]])) 
    stop("full and reduced models must be based on same data")
  
  df.full <- full$df[2]
  df.redu <- reduced$df[2]
  RSSH <- reduced$sigma^2 * df.redu
  RSS <- full$sigma^2 * df.full
  Fstat <- (RSSH - RSS) / (df.redu - df.full) / (full$sigma^2)
  pval <- 1-pf(Fstat,df.redu-df.full,df.full)
  rslt <- c(Fstat, pval,df.redu-df.full,df.full)
  names(rslt) <- c("F stat","p value","num df","den df")
  rslt
}

#' Function to perform a Wald Test on data
#' Used to live in uWaldtest.R
#' 
#' @param full a uRegress object
#' @param constrasts the matrix containing which objects from full to test
#' @param hypothesis the null hypothesis
#' 
#' @return a wald test on the given coefficients
#' 
#' @keywords internal
#' @noRd
uWaldtest <- function (full, contrasts=c(0,rep(1,p-1)), hypothesis=matrix(0,r,1)) {
  
  if (!inherits(full,"uRegress")) stop("not a uRegress object")
  coefficients <- full$coefficients[,1]
  p <- length(coefficients)
  if (!is.matrix(contrasts)) contrasts <- matrix(contrasts,1)
  if (dim(contrasts)[2] != p) stop ("contrasts, coefficient vector must be conformable")
  r <- dim(contrasts)[1]
  if (length(hypothesis) != r) stop ("contrasts, hypothesis must be conformable")
  covmtx <- full$robustCov
  if (is.null(covmtx)) covmtx <- full$naiveCov
  covmtx <- contrasts %*% covmtx %*% t(contrasts)
  Fstat <- t(contrasts %*% coefficients - hypothesis) %*% solve(covmtx) %*% (contrasts %*% coefficients - hypothesis) / r
  if (full$useFdstn) {
    df.den <- full$waldStat[4]
    pval <- 1-pf(Fstat,r,df.den)
    rslt <- c(Fstat, pval,r,df.den)
    names(rslt) <- c("F stat","p value","num df","den df")
  } else {
    pval <- 1-pchisq(Fstat,r,)
    rslt <- c(Fstat, pval,r)
    names(rslt) <- c("Chi2 stat","p value","df")
  }
  rslt	
}

#' Function to get the order of predictors 
#' Used in regress, used to be inside of regress()
#' 
#' @param vec
#' @param n 
#' 
#' @return a wald test on the given coefficients
#' 
#' @keywords internal
#' @noRd
getn <- function(vec, n){
  if(n > length(vec)){
    return(vec)
  } else {
    return(vec[n:length(vec)])
  }
}

#' Inserts a column into a matrix
#' Used to be inside of regress()
#' 
#' @param x
#' @param indx
#' @param col
#' 
#' @return 
#' 
#' @keywords internal
#' @noRd
insertCol <- function(x, indx, col){
  if(length(indx)==1){
    if(indx > dim(x)[2]){
      tmp <- cbind(x, col)
    } else if (indx > 1){
      tmp <- cbind(x[,1:(indx-1)], col, x[,(indx):dim(x)[2]])
    } else {
      tmp <- cbind(col, x)
    }
    return(tmp)
  } else {
    tmp1 <- insertCol(x, indx[1], col)
    tmp2 <- insertCol(tmp1, indx[-1], col)
    return(tmp2)
  }
}

## Function to process terms and get correct order for 
## partial F-tests. 
## Args:  z - the object to store the model matrix and list of terms and predictors in
##     Term - the vector to put in the terms list
## TermName - the name of the given term
## Returns: a list with the current matrix (made with the term), 
##          list of predictors, and order
## Version: 2015 04 29
processTerm <- function (z, Term, TermName) {
  z$terms <- c(z$terms,TermName)
  nTerm <- length(z$terms)
  nPred <- length(z$preds)
  z$firstPred <- c(z$firstPred,nPred+1)
  z$lastPred <- c(z$lastPred,nPred+1)
  if (is.list(Term)) {
    p <- length(Term)
    if (is.null(names(Term))) {
      names(Term) <- paste("T",1:length(Term),sep="")
    }
    for (i in 1:p) {
      z <- processTerm (z, Term[[i]], paste(ifelse(p==1,""," "),TermName,".",names(Term)[i],sep=""))
    }
  } else if (is.matrix(Term)) {
    if (is.null(dimnames(Term)[[2]])) {
      predNms <- paste("V",1:(dim(Term)[2]),sep="")
    } else predNms <- dimnames(Term)[[2]]
    if (!is.null(z$X)) {
      if (dim(Term)[1]!=dim(z$X)[1]) stop("all terms must have same number of cases")
    }
    mode(Term) <- "numeric"
    z$X <- cbind(z$X,Term)
    z$preds <- c(z$preds,paste(ifelse(dim(Term)[2]==1,""," "),TermName,".",predNms,sep=""))
  } else {
    if (!is.null(z$X)) {
      if (length(Term)!=dim(z$X)[1]) stop("all terms must have same number of cases")
    }
    mode(Term) <- "numeric"
    z$X <- cbind(z$X,Term)
    z$preds <- c(z$preds,TermName)
  }
  z$lastPred[nTerm] <- dim(z$X)[2]
  z
}


## function to put the numbers on the names correctly
## based on the length of the coefficients matrix
## Args: str - the string vector to attach the numbers to
##      nums - the numbers vector - will have NAs
attachNums <- function(str, nums){
  largeNoNa <- max(which(!is.na(nums)))
  if(length(nums)<10){ 
    tmp <- paste(ifelse(is.na(nums), rep(" ", length(explode(paste("[",nums[largeNoNa],"] ",sep=" ")))+1), paste("[",nums,"] ",sep="")), str, sep="")
  } else {
    tmp <- ifelse(!is.na(nums), ifelse(nums<10, paste(ifelse(is.na(nums), rep(" ", length(explode(paste("[",nums[largeNoNa],"] ",sep=" ")))+1), paste("[",nums,"] ",sep="")), str, sep=" "),
                                       paste(ifelse(is.na(nums), rep(" ", length(explode(paste("[",nums[largeNoNa],"] ",sep=" ")))+1), paste("[",nums,"] ",sep="")), str, sep="")),
                  paste(rep(" ", length(explode(paste("[",nums[largeNoNa],"] ",sep=" ")))+1), str))
  }
  return(tmp)
}

#' Print method for class uRegress
#' 
#' @param x uRegress object
#' @param augmented whether or not the object has an augmented coefficients matrix
#' @param digits the number of digits to round to
#' @param signif.stars whether to print stars for significant results
#' @param suppress whether or not to suppress printing of the raw coefficients (for example, in logistic regression)
#' 
#' @return a printed uRegress object
#' 
#' @noRd
#' @export
print.uRegress <- function (x,...,augmented=TRUE,digits=max(3,getOption("digits")-3),signif.stars=FALSE, suppress) {
  
  suppress <- x$suppress
  
  if (!is.null(dim(x$model))) {
    tmp <- dimnames(x$model)[[1]]
    tmp <- indentNames(tmp, x$coefNums, x$levels)
    tmp <- attachNums(tmp, x$coefNums)
    dimnames(x$model)[[1]] <- tmp
    if(!is.na(x$transformed)[1]){
      dimnames(x$transformed)[[1]] <- tmp
    }
  } else {
    tmp <- names(x$model)
    tmp3 <- names(x$transformed)
    tmpMat <- matrix(x$model, nrow=1)
    tmpMat2 <- matrix(x$transformed, nrow=1)
    tmp2 <- dimnames(x$model)[[1]]
    tmp2 <- indentNames(tmp2, x$coefNums, x$levels)
    tmp2 <- attachNums(tmp2, x$coefNums)
    x$model <- tmpMat
    x$transformed <- tmpMat2
    dimnames(x$model) <- list(tmp2, tmp)
    dimnames(x$transformed) <- list(tmp2,tmp3)
  }
  
  x$augmented <- augmented
  x$suppress <- suppress
  #	cat("n =",x$n)
  if(!is.null(x$na.action)) {
    cat("(",length(x$na.action)," cases deleted due to missing values)")
    cat("\n\n")
  }
  if(!x$anyRepeated){
    if (x$fnctl %in% c("mean","geometric mean")) {
      #f <- getAnywhere(print.summary.lm)$objs[[1]]
      f <- printerLm
    } else f <- printerGlm
  } 
  if (augmented) {
    x$coefficients <- x$augCoefficients
    x$conf.int <- NULL
  }
  f(x,digits=digits,signif.stars=signif.stars, suppress=suppress)
}

## A helper function for print.uRegress()
## Properly indents the augmentedCoefficients matrix
## Args: nms      - the names of the matrix of interest
##       coefNums - the vector of coefficient numbers
## Returns: a vector with the indented names
## Version: 2015 05 25
indentNames <- function(nms, coefNums, levels){
  ## they come in with one space, we need 3 more to get to even with intercept
  nms[is.na(coefNums)] <- paste("   ", nms[is.na(coefNums)], sep="")
  nmMat <- cbind(nms, levels)
  ## add two*levels spaces to each
  ## x is a vector
  addSpaces <- function(x){
    if(x[2]>0){
      y <- paste(paste(rep(" ", 2*as.numeric(x[2])), collapse=""), x[1], sep="")
    } else {
      y <- x[1]
    }
    return(y)
  }
  newNms <- apply(nmMat, 1, addSpaces)
  return(newNms)
}
## A helper function for regress()
## Gets the levels to properly indent the coefficients matrix
## Args: nms      - the names of the matrix of interest
##       coefNums - the vector of coefficient numbers
##       termnms  - the names of the nested calls
##       level    - the current level of the function
## Returns: a vector with the indendation structure
## Version: 2015 05 25

getLevels <- function(nms, coefNums, termnms, term.lbls, level){
  ## initialize all levels to level
  levels <- rep(level, length(nms))
  
  ## Check for nested terms
  nested <- grepl(" ", nms, fixed=TRUE)
  dot <- grepl(".", nms, fixed=TRUE)
  carrot <- grepl("^", nms, fixed=TRUE)
  u <- grepl("U", nms)
  nested[u] <- FALSE
  
  ## is a lspline or dummy
  tmp <- NULL
  tmp2 <- NULL
  if(any(dot)){
    tmp <- nested
    tmp[!dot] <-  FALSE
  } 
  if(any(carrot)){
    tmp2 <- nested
    tmp2[!carrot] <- FALSE
  }
  if(!is.null(tmp)){
    if(is.null(tmp2)){
      nested <- tmp
    } else {
      nested[!dot&!carrot] <- FALSE
    }
  } else if (!is.null(tmp2)){
    if(is.null(tmp)){
      nested <- tmp2
    } else {
      nested[!dot&!carrot] <- FALSE
    }
  }
  
  ## set nested to one higher than level
  levels[nested] <- levels[nested]+1
  
  ## roll through vector and bump up level when necessary
  for(i in 1:length(levels)){
    ## if coefNum is a number, keep level the same
    if(!is.na(coefNums[i]) & levels[i] <= level){
      levels[i] <- level
    } else {
      ## check to see if this is in the names of terms
      termCheck <- sapply(termnms, match, nms[i])
      ## check to see if it is in term labels and has no nestings following
      lblCheck <- sapply(term.lbls, match, nms[i])
      lblCheck2 <- sapply(nms[i], grepl, term.lbls, fixed=TRUE)
      if(is.list(lblCheck2)){
        lblCheck2 <- unlist(lblCheck2)
      }
      if(any(!is.na(termCheck))){ ## it is a nested test
        ## set its level to current level 
        if(!is.na(termCheck[1])){
          levels[i] <- level
          level <- level+1 
        } else if(!any(!is.na(lblCheck))) {
          levels[i] <- level-1
        } else {
          levels[i] <- level
          level <- level+1
        }
        ## increase the level
      } else { ## it is a regular variable
        ## if nested, we have already taken care of it
        if(!nested[i]){
          ## set level to current level
          levels[i] <- level
          if(!nested[i+1] & any(lblCheck2) & grepl(":", nms[i])){
            levels[i] <- level-1
          }
          ## bump adjacent nested
          j <- i+1
          while(nested[j] & j <= length(nested)){
            levels[j] <- level + 1
            j <- j+1
          }
        }
      }
    }
  }
  return(levels)
}

# helper function for print.uRegress, used to be inside of print.uRegress()
printerLm <- function (x, digits = max(3L, getOption("digits") - 3L), symbolic.cor = x$symbolic.cor, 
                       signif.stars = getOption("show.signif.stars"), suppress, ...) 
{
  if(!is.null(x$augmented)){
    augmented <- x$augmented
  } else{
    augmented <- FALSE
  }
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  resid <- x$residuals
  df <- x$df
  rdf <- df[2L]
  if(!is.null(resid)){
    cat(if (!is.null(x$weights) && diff(range(x$weights))) 
      "Weighted ", "Residuals:\n", sep = "")
    if (rdf > 5L) {
      nam <- c("Min", "1Q", "Median", "3Q", "Max")
      rq <- if (length(dim(resid)) == 2L) 
        structure(apply(t(resid), 1L, quantile), dimnames = list(nam, 
                                                                 dimnames(resid)[[2L]]))
      else {
        zz <- zapsmall(quantile(resid), digits + 1L)
        structure(zz, names = nam)
      }
      print(rq, digits = digits, ...)
    }
    else if (rdf > 0L) {
      print(resid, digits = digits, ...)
    }
    else {
      cat("ALL", df[1L], "residuals are 0: no residual degrees of freedom!")
      cat("\n")
    }
  }
  if (length(x$aliased) == 0L) {
    cat("\nNo Coefficients\n")
  }
  else {
    if (nsingular <- df[3L] - df[1L]) 
      cat("\nCoefficients: (", nsingular, " not defined because of singularities)\n", 
          sep = "")
    else cat("\nCoefficients:\n")
    coefs <- x$coefficients
    
    if(augmented){
      if(is.na(x$transformed[1])){
        print.augCoefficients(x$model)
      } else if (suppress){
        cat("\nTransformed Model:\n")
        print.augCoefficients(x$transformed)
      } else {
        cat("\nRaw Model:\n")
        print.augCoefficients(x$model)
        cat("\nTransformed Model:\n")
        print.augCoefficients(x$transformed)
      }
    } else{
      if(is.na(x$transformed[1])){
        printCoefmat(x$model, digits = digits, signif.stars = signif.stars, 
                     na.print = "NA", ...)
      } else if (suppress){
        cat("\n Transformed Model: \n")
        printCoefmat(x$transformed, digits = digits, signif.stars = signif.stars, 
                     na.print = "NA", ...)
      } else {
        cat("\n Raw Model: \n")
        printCoefmat(x$model, digits = digits, signif.stars = signif.stars, 
                     na.print = "NA", ...)
        cat("\n Transformed Model: \n")
        printCoefmat(x$transformed, digits = digits, signif.stars = signif.stars, 
                     na.print = "NA", ...)
        
      }
    }
  }
  for(i in 1:length(x$args)){
    if(is.list(x$args[[i]])){
      if(x$args[[i]]$transformation=="polynomial"){
        cat("\n", paste("Polynomial terms calculated from ", x$args[[i]]$nm, ", centered at ",round(x$args[[i]]$center, digits), sep=""),  "\n")
      } else if (x$args[[i]]$transformation=="lspline"){
        cat("\n", paste("Linear Spline terms calculated from ", x$args[[i]]$nm, ", knots = ",paste(x$args[[i]]$knots, collapse=","),", param = ", x$args[[i]]$param, sep=""), "\n")
      } else {
        cat("\n", paste("Dummy terms calculated from ", x$args[[i]]$nm, ", reference = ",x$args[[i]]$reference,sep=""), "\n")
      }
    }
  }
  if(any(x$dropped)){
    cat("\n", "Predictor(s) dropped due to overfitting", "\n")
  }
  if(!is.null(x$sigma)){
    cat("\nResidual standard error:", format(signif(x$sigma, 
                                                    digits)), "on", rdf, "degrees of freedom")
    cat("\n")
    if (nzchar(mess <- naprint(x$na.action))) 
      cat("  (", mess, ")\n", sep = "")
    if (!is.null(x$fstatistic)) {
      cat("Multiple R-squared: ", formatC(x$r.squared, digits = digits))
      cat(",\tAdjusted R-squared: ", formatC(x$adj.r.squared, 
                                             digits = digits), "\nF-statistic:", formatC(x$fstatistic[1L], 
                                                                                         digits = digits), "on", x$fstatistic[2L], "and", 
          x$fstatistic[3L], "DF,  p-value:", format.pval(pf(x$fstatistic[1L], 
                                                            x$fstatistic[2L], x$fstatistic[3L], lower.tail = FALSE), 
                                                         digits = digits))
      cat("\n")
    }
    correl <- x$correlation
    if (!is.null(correl)) {
      p <- NCOL(correl)
      if (p > 1L) {
        cat("\nCorrelation of Coefficients:\n")
        if (is.logical(symbolic.cor) && symbolic.cor) {
          print(symnum(correl, abbr.colnames = NULL))
        }
        else {
          correl <- format(round(correl, 2), nsmall = 2, 
                           digits = digits)
          correl[!lower.tri(correl)] <- ""
          print(correl[-1, -p, drop = FALSE], quote = FALSE)
        }
      }
    }
  }
  
  cat("\n")
  invisible(x)
}

# helper function for print.uRegress, used to be inside of print.uRegress()
printerGlm <- function (x, digits = max(3L, getOption("digits") - 3L), symbolic.cor = x$symbolic.cor, 
                        signif.stars = getOption("show.signif.stars"), suppress, ...) 
{
  if(!is.null(x$augmented)){
    augmented <- x$augmented
  } else{
    augmented <- FALSE
  }
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  cat("Deviance Residuals: \n")
  if (x$df.residual > 5) {
    x$deviance.resid <- setNames(quantile(x$deviance.resid, 
                                          na.rm = TRUE), c("Min", "1Q", "Median", "3Q", "Max"))
  }
  xx <- zapsmall(x$deviance.resid, digits + 1L)
  print.default(xx, digits = digits, na.print = "", print.gap = 2L)
  if (length(x$aliased) == 0L) {
    cat("\nNo Coefficients\n")
  }
  else {
    df <- if ("df" %in% names(x)) 
      x[["df"]]
    else NULL
    if (!is.null(df) && (nsingular <- df[3L] - df[1L])) 
      cat("\nCoefficients: (", nsingular, " not defined because of singularities)\n", 
          sep = "")
    else cat("\nCoefficients:\n")
    if(augmented){
      if(is.na(x$transformed[1])){
        print.augCoefficients(x$model)
      } else if (suppress){
        cat("\nTransformed Model:\n")
        print.augCoefficients(x$transformed)
      } else {
        cat("\nRaw Model:\n")
        print.augCoefficients(x$model)
        cat("\nTransformed Model:\n")
        print.augCoefficients(x$transformed)
      }
    } else{
      if(is.na(x$transformed[1])){
        printCoefmat(x$model, digits = digits, signif.stars = signif.stars, 
                     na.print = "NA", ...)
      } else if (suppress){
        cat("\nTransformed Model:\n")
        printCoefmat(x$transformed, digits = digits, signif.stars = signif.stars, 
                     na.print = "NA", ...)
      } else {
        cat("\nRaw Model:\n")
        printCoefmat(x$model, digits = digits, signif.stars = signif.stars, 
                     na.print = "NA", ...)
        cat("\nTransformed Model:\n")
        printCoefmat(x$transformed, digits = digits, signif.stars = signif.stars, 
                     na.print = "NA", ...)
        
      }
    }
  }
  for(i in 1:length(x$args)){
    if(is.list(x$args[[i]])){
      if(x$args[[i]]$transformation=="polynomial"){
        cat("\n", paste("Polynomial terms calculated from ", x$args[[i]]$nm, ", centered at ",round(x$args[[i]]$center, digits), sep=""),  "\n")
      } else if (x$args[[i]]$transformation=="lspline"){
        cat("\n", paste("Linear Spline terms calculated from ", x$args[[i]]$nm, ", knots = ",x$args[[i]]$knots,", param = ", x$args[[i]]$param, sep=""), "\n")
      } else {
        cat("\n", paste("Dummy terms calculated from ", x$args[[i]]$nm, ", reference = ",x$args[[i]]$reference,sep=""), "\n")
      }
    }
  }
  if(any(x$dropped)){
    cat("\n", "Predictor(s) dropped due to overfitting", "\n")
  }
  cat("\n(Dispersion parameter for ", x$family$family, " family taken to be ", 
      format(x$dispersion), ")\n\n", apply(cbind(paste(format(c("Null", 
                                                                "Residual"), justify = "right"), "deviance:"), format(unlist(x[c("null.deviance", 
                                                                                                                                 "deviance")]), digits = max(5L, digits + 1L)), " on", 
                                                 format(unlist(x[c("df.null", "df.residual")])), " degrees of freedom\n"), 
                                           1L, paste, collapse = " "), sep = "")
  if (nzchar(mess <- naprint(x$na.action))) 
    cat("  (", mess, ")\n", sep = "")
  cat("AIC: ", format(x$aic, digits = max(4L, digits + 1L)), 
      "\n\n", "Number of Fisher Scoring iterations: ", x$iter, 
      "\n", sep = "")
  correl <- x$correlation
  if (!is.null(correl)) {
    p <- NCOL(correl)
    if (p > 1) {
      cat("\nCorrelation of Coefficients:\n")
      if (is.logical(symbolic.cor) && symbolic.cor) {
        print(symnum(correl, abbr.colnames = NULL))
      }
      else {
        correl <- format(round(correl, 2L), nsmall = 2L, 
                         digits = digits)
        correl[!lower.tri(correl)] <- ""
        print(correl[-1, -p, drop = FALSE], quote = FALSE)
      }
    }
  }
  cat("\n")
  invisible(x)
}

print.augCoefficients <-
  function (x,...,sigfigs=max(3,getOption("digits")-3),width=9,nonsci.limit=5,Psci=FALSE, version=FALSE) {
    
    vrsn <- "20110928"
    if (version) return(vrsn)
    
    
    #
    # prints CI or exp(CI) to specified sigfigs
    # rounds exp(Est) to same number of decimal figures as exp(CI)
    # prints Std Err or Robust SE to specified sigfigs
    # rounds Estimate to same number of decimal figures as StdErr or Robust SE
    # prints F stat to specified sigfigs
    # prints P value to number of decimal figures as nonsci.limit unless < 10^-nonsci.limit when "< .00001" used
    # centers all but df and P value, which is right justified
    
    cmptRoundDigits <- function (x, sf) {
      y <- max(abs(x),na.rm=T)
      if (y==0) {
        sf
      } else {
        y <- trunc(log(y) / log(10)) - (y < 1)
        max(0,sf - y - (y < sf))
      }
    }
    
    frmtCol <- function (x, sf, nonsci.limit, colwidth=9) {
      rslt <- NULL
      for (i in 1:length(x)) {	
        if (is.na(x[i])) {
          tmp <- "NA"
        } else {
          rd <- cmptRoundDigits (x[i], sf)
          if (rd <= nonsci.limit & abs(x[i]) < 10^nonsci.limit) {
            tmp <- format(round(x[i],rd),nsmall=rd,width=1)
          } else {
            tmp <- format(round(x[i],rd), digits=sf, scientific=T, width=1)
          }
        }
        rslt <- c(rslt,ifelse(x[i]<0,tmp,paste(" ",tmp,sep="")))
      }
      format(rslt, justify="centre", width=colwidth)
    }
    
    frmtCoefficients <- format(x)
    Pvalcol <- dim(x)[2]
    for (j in 1:(Pvalcol-3)) frmtCoefficients[,j] <- frmtCol (x[,j],sigfigs,nonsci.limit,width)
    SEcol <- ifelse(dimnames(x)[[2]][3]=="RobustSE",3,2)
    frmtCoefficients[,SEcol+1] <- paste(frmtCoefficients[,SEcol+1],"  ")
    frmtCoefficients[,Pvalcol-2] <- paste("  ",format(round(x[,Pvalcol-2],2),width=width,justify="right"))
    dimnames(frmtCoefficients)[[2]][Pvalcol-2] <- paste("  ",dimnames(frmtCoefficients)[[2]][Pvalcol-2])
    frmtCoefficients[,Pvalcol-1] <- format(x[,Pvalcol-1])
    if (Psci) {
      frmtCoefficients[,Pvalcol] <- format(x[,Pvalcol],digits=sigfigs,scientific=T,width=width,justify="centre")
    } else{
      z <- paste(format(round(x[,Pvalcol],4),width=width-1,justify="right")," ",sep="")
      z[x[,Pvalcol] < 5e-5] <- format("< 0.00005",width=width,justify="right")
      frmtCoefficients[,Pvalcol] <- format(z,justify="right",width=width)
    }
    u <- is.na(x[,1])
    if (any(u)) frmtCoefficients[u,1:(Pvalcol-3)] <- ""
    dimnames(frmtCoefficients)[[1]] <- paste(dimnames(frmtCoefficients)[[1]],"  ")
    print(frmtCoefficients,quote=F)
    invisible(frmtCoefficients)
  }

fitted.uRegress <-
  function (object,...,X) {
    
    obj <- object
    
    if (!missing(X)) {
      if (dim(X)[2] != dim(obj$processTerms$X)[2]) stop("new data not appropriate dimensions")
      if (dim(obj$coefficients)[1] == dim(X)[2] + 1) X <- cbind(1,X)
      fits <- X %*% obj$coefficients[,1]
    } else fits <- obj$glmobj$fitted.values
    if (obj$functional == "geometric mean") fits <- exp(fits)
    fits
  }

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
#' the class of \code{object$fit}. See \code{\link[stats]{predict.lm}}, 
#' or \code{\link[stats]{predict.glm}} for
#' more details. 
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
  } else {
    ret <- predict.glm(object$fit,interval=interval,level=level,...)
  } 
  return(ret)
}

