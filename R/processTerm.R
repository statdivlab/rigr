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