print.uCorrelate <-
function (x, ..., stat=attr(x,"stat"), byStratum=attr(x,"byStratum"),
                              sigfigs=max(5,getOption("digits")-2),width=9,nonsci.limit=5, version=F) {
  
  vrsn <- "20110930"
  if (version) return(vrsn)
  
  cmptRoundDigits <- function (x, sf) {
    u <- !is.na(x) & x>-Inf & x < Inf
    y <- if (any(u)) max(abs(x[u]),na.rm=T) else 0
    if (y==0) {
      1
    } else {
      y <- trunc(log(y) / log(10)) - (y < 1)
      max(0,sf - y - (y < sf))
    }
  }
  
  frmtCol <- function (x, rd, sf, nonsci.limit, colwidth=9, append="") {
    rslt <- NULL
    for (i in 1:length(x)) {	
      if (is.nan(x[i])) {
        tmp <- "NaN"
      } else if (is.na(x[i])) {
        tmp <- "NA"
      } else {
        if (rd <= nonsci.limit & abs(x[i]) < 10^nonsci.limit) {
          tmp <- format(round(x[i],rd),nsmall=rd,width=1)
        } else {
          tmp <- format(round(x[i],rd), digits=sf, scientific=T, width=1)
        }
        tmp <- ifelse(x[i]<0,tmp,paste(" ",tmp,sep=""))
      }
      rslt <- c(rslt,tmp)
    }
    rslt <- paste(rslt,append,sep="")
    format(rslt, justify="right", width=colwidth)
  }
  
  parseFormat <- function (x) {
    z <- strsplit(x,"@")[[1]]
    p <- length(z)
    stat <- z[seq(2,p,2)]
    specialFormat <- z[seq(1,p,2)]
    list(stat=stat,specialFormat=specialFormat)
  }
  
  removeLeading <- function (x) {
    allHaveBlank <- T
    while (allHaveBlank) {
      first <- substring(x,1,1)
      if (any(first!=" ")) allHaveBlank <- F
      if (allHaveBlank) x <- substring(x,2)
    }
    x
  }
  
  removeTrailing <- function (x) {
    allHaveBlank <- T
    while (allHaveBlank) {
      first <- substring(x,nchar(x),nchar(x))
      if (any(first!=" ")) allHaveBlank <- F
      if (allHaveBlank) x <- substring(x,1,nchar(x)-1)
    }
    x
  }
  
  removeLeadingTrailing <- function (x) {
    x <- removeLeading (x)
    x <- removeTrailing (x)
    x
  }
  
  if (!inherits(x,"uCorrelate")) stop("x must be a uCorrelate object")
  conf.level <- attr(x,"conf.level")
  stat.chc <- c("cor", "n", "t.stat", "pval", "loCI", "hiCI","all")
  Labels <- c("Estimated Correlation Coefficients","Sample Size","t Statistics","Two-sided P values",
              paste(format(100*conf.level), "% Conf Intvl: Lower",sep=""),
              paste(format(100*conf.level), "% Conf Intvl: Upper",sep=""))
  xindx <- 1:6
  specialLabel <- list("Corr","N","t Stat","Two-sided P","loCI","hiCI")
  
  if (!byStratum & length(x) > 1) {
    v <- NULL
    for (k in 1:(dim(x[[1]][[1]])[1])) {
      vrblTables <- NULL
      for (i in 1:6) {
        vt <- NULL
        for (j in 1:length(x)) {
          vt <- rbind(vt,x[[j]][[i]][k,])
        }
        dimnames(vt)[[1]] <- names(x)
        vrblTables <- c(vrblTables,list(vt))
      }
      names(vrblTables) <- names(x[[1]])
      v <- c(v,list(vrblTables))
    }
    names(v) <- dimnames(x[[1]][[1]])[[1]]
  } else v <- x
  
  
  for (i in 1:(length(v))) {
    for (k in 1:6) {
      frmtTmp <- format(v[[i]][[k]])
      rnd <- cmptRoundDigits(as.vector(v[[i]][[k]]),sf=ifelse1(k==2,1,sigfigs))
      frmtTmp[1:length(frmtTmp)] <- removeLeadingTrailing (frmtCol(as.vector(v[[i]][[k]]), rnd, 
                                                                   sf=ifelse1(k==2,1,sigfigs), nonsci.limit, 1, ""))
      v[[i]][[k]] <- frmtTmp
    }
  }
  
  chc.use <- c("everything","complete.obs","pairwise.complete.obs")
  use <- pmatch(attr(x,"use"), chc.use)
  chc.method <- c("pearson","spearman")
  method <- pmatch(attr(x,"method"),chc.method)
  cat("Tabled correlation statistics by strata\nCall:\n     ",format(attr(x,"call")),"\n")
  cat("     Method: ",c("Pearson","Spearman")[method],"\n")
  cat("     Data  : ",c("All","Complete Cases","Pairwise Complete")[use],"\n")
  cat("            - NaN denotes strata with no observations\n")
  cat("            - NA arises from missing data\n")
  statTable <- NULL
  
  specialFormat <- NULL
  if (length(stat)==1) {
    if (is.na(pmatch(stat,stat.chc))) {
      z <- parseFormat (stat)
      if (length(z$stat) > 0) {
        stat <- z$stat
        specialFormat <- c(z$specialFormat,"")
      }
    } else if (stat=="all" | stat=="al" | stat=="a") stat <- stat.chc[1:6]
  }
  
  for (j in 1:length(v)) {
    template <- NULL
    term <- 0
    stratumTables <- NULL
    if (byStratum | length(v)==1) {
      if (j < length(v)) cat("\n##### Stratum ",names(v)[j],"\n")
      else cat("\n##### ALL DATA\n")
    } else cat("\n##### Variable ",names(v)[j],"\n")	
    for (s in stat) {
      indx <- pmatch(s,stat.chc)
      if (is.na(indx)) {
        warning(paste(s,"is unrecognized stat"))
      } else {
        if (is.null(specialFormat)) {
          cat("   ## ",Labels[indx],"\n")
          print(v[[j]][[xindx[indx]]],quote=F)
          stratumTables <- c(stratumTables,list(v[[j]][[xindx[indx]]]))
        } else {
          tmp <- v[[j]][[xindx[indx]]]
          sL <- specialLabel[[indx]]
          term <- term + 1
          if (term == 1) {
            tmp[1:length(tmp)] <- paste(specialFormat[term],as.vector(tmp),sep="")
            stratumTables <- tmp
            template <- paste(specialFormat[term],sL,sep="")
          } else {
            stratumTables[1:length(stratumTables)] <- paste(as.vector(stratumTables),specialFormat[term],as.vector(tmp),sep="")
            template <- paste(template,specialFormat[term],sL,sep="")
          }
        }
      }
    }
    if (!is.null(specialFormat)) {
      term <- term + 1
      stratumTables[1:length(stratumTables)] <- paste(as.vector(stratumTables),specialFormat[term],sep="")
      template <- paste(template,specialFormat[term],sep="")
      cat("\nFormat: ",template,"\n\n")
      print(stratumTables, quote=F)
    }
    statTable <- c(statTable,list(stratumTables))
  }
  names(statTable) <- names(x)
  invisible(statTable)
}
