## Function to fully parse a formula for regress()
## Takes in formula, potentially with nested F-tests
## Returns a list of lists of tests to perform (and overall formula)
## Args:    form       - the formula to parse
##          modelframe - the model frame which will become the model matrix
##          mat        - matrix of logicals to run on
##          data       - the data frame
## Returns: a list with two elements; formula - the full formula
##                                   testList - the tests to run
## Version: 20150522
testList <- function(form, modelframe, mat, data){
  ## Takes a formula, returns a list of terms
  ## Args: form - the formula
  ## Returns: the list of terms
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
  ## Takes a formula, returns a viable formula 
  ## Args: form - the formula
  ## Returns: the list
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
  ## Go through a parseFactor and get the relevant formulas and partials
  ## Args: list - a list returned by parseFactor
  ## Returns: a list with two components - the formula and a list of all of the matrices
  parseParseFormula <- function(lst){
    tmp <- unlist(lst)
    parsetmp <- sapply(tmp, deparse)
    len <- floor(length(parsetmp)/2)
    oper <- rev(parsetmp[1:len])
    
    vec <- matrix(parsetmp[(len+1):length(parsetmp)], nrow=1)
    ## inserts a vector into another vector
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
    ret <- insertVec(vec, oper, 2)
    return(ret)
  }
  LinearizeNestedList <- function(NList, LinearizeDataFrames=FALSE,
                                  NameSep="/", ForceNames=FALSE) {
    # LinearizeNestedList:
    #
    # https://sites.google.com/site/akhilsbehl/geekspace/
    #         articles/r/linearize_nested_lists_in_r
    #
    # Akhil S Bhel
    # 
    # Implements a recursive algorithm to linearize nested lists upto any
    # arbitrary level of nesting (limited by R's allowance for recursion-depth).
    # By linearization, it is meant to bring all list branches emanating from
    # any nth-nested trunk upto the top-level trunk s.t. the return value is a
    # simple non-nested list having all branches emanating from this top-level
    # branch.
    #
    # Since dataframes are essentially lists a boolean option is provided to
    # switch on/off the linearization of dataframes. This has been found
    # desirable in the author's experience.
    #
    # Also, one'd typically want to preserve names in the lists in a way as to
    # clearly denote the association of any list element to it's nth-level
    # history. As such we provide a clean and simple method of preserving names
    # information of list elements. The names at any level of nesting are
    # appended to the names of all preceding trunks using the `NameSep` option
    # string as the seperator. The default `/` has been chosen to mimic the unix
    # tradition of filesystem hierarchies. The default behavior works with
    # existing names at any n-th level trunk, if found; otherwise, coerces simple
    # numeric names corresponding to the position of a list element on the
    # nth-trunk. Note, however, that this naming pattern does not ensure unique
    # names for all elements in the resulting list. If the nested lists had
    # non-unique names in a trunk the same would be reflected in the final list.
    # Also, note that the function does not at all handle cases where `some`
    # names are missing and some are not.
    #
    # Clearly, preserving the n-level hierarchy of branches in the element names
    # may lead to names that are too long. Often, only the depth of a list
    # element may only be important. To deal with this possibility a boolean
    # option called `ForceNames` has been provided. ForceNames shall drop all
    # original names in the lists and coerce simple numeric names which simply
    # indicate the position of an element at the nth-level trunk as well as all
    # preceding trunk numbers.
    #
    # Returns:
    # LinearList: Named list.
    #
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
  ## Takes in a linearized list, returns a list with only the non-null components
  parseList <- function(lst){
    indx <- !sapply(lst, is.null)
    tmp <- lst[indx]
    nms <- names(tmp)
    explode <- function(str){
      chars <- strsplit(str, "")[[1]]
      return (chars)
    }
    nms <- sapply(nms, explode, simplify="array")
    getindx <- function(strlst){
      return(sapply(strlst, function(x) x=="U"))
    }
    getnm <- function(strmat, indx){
      newmat <- paste(strmat[indx[1]:dim(strmat)[1],1], collapse="")
      if(length(indx)>1){
        for(i in 2:length(indx)){
          newmat <- c(newmat, paste(strmat[indx[i]:dim(strmat)[1],i], collapse=""))
        }
      }
      return(newmat)
    }
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
  
  ## Takes a formula, returns a list of models for multiple partial tests
  ## Args: form - the formula
  ## Returns: the list
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