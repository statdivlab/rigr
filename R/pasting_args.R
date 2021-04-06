## Function to add the correct args onto a polynomial, dummy, or lspline term
## Version: 2015 05 31
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

## function to paste on degree
pasteOn <- function(x, str, num){
  if(length(x)==1){
    return(paste(x, str, num, sep=""))
  } else{
    ret <- paste(x[1], str, num, sep="")
    ret2 <- pasteOn(x[-1], str, num-1)
    return(c(ret, ret2))
  }
}

## helper function for lspline
pasteOnSpline <- function(x, nums){
  if(length(x)==1){
    return(paste(x, ".K(", nums, ")", sep=""))
  } else{
    ret <- paste(x[1], ".K(", nums[1], ")", sep="")
    ret2 <- pasteOnSpline(x[-1], nums[-1])
    return(c(ret, ret2))
  }
}

## Takes a pair and pastes together, with colon
pastePair <- function(vec){
  if(length(vec)==2){
    return(paste(vec[1], ":", vec[2], sep=""))
  } else{
    ret1 <- paste(vec[1], ":", vec[2], sep="")
    ret2 <- pastePair(vec[-c(1,2)])
    return(c(ret1, ret2))
  }
}
## sums across a logical vector, returns the current value at each point
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
myNext <- function(num, vec){
  if(which(vec==num)+1<= length(vec)){
    ret <- vec[which(vec==num)+1]
  } else {
    ret <- which(vec==num)+1
  }
  return(ret)
}
## Function to tack on args and return appropriate names for printing
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

## function to create the correct columns for processTerm
## takes a predictor and the terms vector, determines which it is
## if an interaction, checks both
## don't put in the intercept
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