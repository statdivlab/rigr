## A function to traverse the termlist tree,
## and create the correct augCoefficients matrix
## A helper function for regress()
## Args: termlist - the list with terms
##       mat      - the matrix to correct
## Returns: A list with the updated augmented coefficients matrix and the current indices
## Version: 2015 05 25
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
## A helper function for termTraverse
## Args: current - the current terms
##       lbls    - the labels of the terms
##       mat     - the augmented coefficients matrix
##       ind     - the current indices
## Returns: A list with an updated augmented coefficients matrix and the current indices
## Version: 2015 05 25
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


