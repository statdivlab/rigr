tabModel <-
function (..., version=F) {
  
  vrsn <- "20140815"
  if (version) return(vrsn)
  
  procTerm <- function (z, Term, TermName) {
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
        z <- procTerm (z, Term[[i]], paste(ifelse(p==1,""," "),TermName,".",names(Term)[i],sep=""))
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
  
  cl <- match.call()
  L <- list(...)
  namesL <- as.character(unlist(match.call(expand.dots=F)$...))
  hyperNames <- as.character(names(match.call(expand.dots=F)$...))
  if (length(L)==1 & is.matrix(L[[1]])) {
    if (dim(L[[1]])[2]==1) {
      if(!is.null(dimnames(L[[1]]))) {
        nms <- dimnames(L[[1]])[[2]]
        if (!is.null(nms)) {
          namesL[1] <- nms
          L[[1]] <- as.vector(L[[1]])
        }
      }
    }
  }
  if (!is.null(hyperNames)) namesL[hyperNames!=""] <- hyperNames[hyperNames!=""]
  names(L) <- namesL
  p <- length(L)
  if(p == 1 & is.list(L[[1]])) L <- L[[1]]
  z <- list(call=cl, terms=NULL,firstPred=NULL,lastPred=NULL,preds=NULL,X=NULL)
  for (i in 1:p) {
    z <- procTerm (z, L[[i]], namesL[i])
  }
  z$preds <- paste(z$preds,
                   ifelse(duplicated(z$preds),paste(".dup",cumsum(duplicated(z$preds)),sep=""),rep("",length(z$preds))),sep="")
  z$terms <- paste(z$terms,
                   ifelse(duplicated(z$terms),paste(".dup",cumsum(duplicated(z$terms)),sep=""),rep("",length(z$terms))),sep="")
  dimnames(z$X)[[2]] <- z$preds
  numZ <- dim(z$X)[2]
  zList <- list()
  for(i in 1:numZ){
    zList[[i]] <- z$X[,i]
  }
  names(zList) <- z$terms
  z[[6]] <- zList
  class (z) <- "tabModel"
  z
}
