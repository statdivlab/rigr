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
