print.tableStat <-
function (x, ...,stat=attr(x,"stat"), na.rm=attr(x,"na.rm"),
                             sigfigs=max(3,getOption("digits")-3),width=9,nonsci.limit=5, print.it=TRUE, printer = attr(x, "printer"), version=FALSE) {
  
  vrsn <- "20121026"
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
  
  if (!inherits(x,"tableStat")) stop("x must be a tableStat object")
  isDate <- attr(x,"isDate")
  stat.chc <- c("count", "missing", "mean", "geometric mean", "median", "sd", "variance", "minimum", "maximum", "quantiles",
                "probabilities", "mn(sd)", "range", "iqr","all","row%", "col%", "tot%")
  Labels <- c("Counts of","Number of missing","Mean of","Geometric mean","Median of","Standard deviation of",
              "Variance of","Minima of","Maxima of","Quantiles of","Probabilities for specified ranges of",
              "Mean (SD) of","Min, Max of","25th, 75th %iles of","","Percentages by Row of","Percentages by Column of","Percentages (overall) of")
  xindx <- list(1,2,3,5,(1:length(x))[names(x)==" Mdn"],4,4,6,(1:length(x))[names(x)==" Max"],
                7:((1:length(x))[names(x)==" Max"]-1),((1:length(x))[names(x)==" Max"]+1):(length(x)-4),
                3:4,c(6,(1:length(x))[names(x)==" Max"]),
                c((1:length(x))[names(x)==" 25%"],(1:length(x))[names(x)==" 75%"]))
  if (length(xindx[[10]]) == 2 && xindx[[10]][1] > xindx[[10]][2]) xindx[[10]] <- rep(NA,2)
  if (length(xindx[[11]]) == 2 && xindx[[11]][1] > xindx[[11]][2]) xindx[[11]] <- rep(NA,2)
  if (length(xindx[[5]])==0) xindx[[5]] <- NA
  if (length(xindx[[14]])!=2) xindx[[14]] <- NA
  specialLabel <- list("Cnt","Msng","Mean","GeomMn","Mdn","SD",
                       "Vrnc","Min","Max",names(x)[xindx[[10]]],names(x)[xindx[[11]]],
                       "Mn(SD)","(Min, Max)","(25%, 75%)","","% of row","% of col","% of total")
  if (any(stat %in% c("a","al","all"))) stat <- stat.chc[-c(5,length(stat.chc))]
  
  if (!na.rm) {
    for (i in 3:length(x)) x[[i]][x[[2]]>0 & !is.nan(x[[2]])] <- NA
  } else {
    x[[1]] <- x[[1]]-x[[2]]
  }
  
  rowPct <- x[[1]]
  if (is.vector(rowPct)) {
    rowPct <- 100 * rowPct / rowPct[length(rowPct)]
  } else {
    dimTable <- dim(rowPct)
    tblSize <- dimTable[1] * dimTable[2]
    indx <- 1:length(rowPct)
    tblNbr <- (indx - 1) %/% tblSize
    tblRow <- rep(1:(dimTable[1]),length.out=length(indx))
    denomIndx <- (tblNbr * tblSize) + (tblSize - dimTable[1]) + tblRow
    rowPct <- 100 * rowPct / rowPct[denomIndx]
  }
  colPct <- x[[1]]
  if (is.vector(colPct)) {
    colPct <- 100 * colPct / colPct
  } else {
    denomIndx <- rep(seq(dimTable[1],length(colPct),by=dimTable[1]),each=dimTable[1])
    colPct <- 100 * colPct / colPct[denomIndx]
  }
  totPct <- 100 * x[[1]] / x[[1]][length(x[[1]])]
  
  censMin <- x[["firstEvent"]] > x[[6]]
  censMin[is.na(censMin)] <- F
  censMax <- x[["lastEvent"]] < x[[xindx[[9]]]]
  censMax[is.na(censMax)] <- F
  if (any(censMax)) {
    rAppend <- ifelse(censMax,"+"," ")
    maxAppend <- ifelse(censMax,"+"," ")
  } else {
    rAppend <- NULL
    maxAppend <- NULL
  }
  if (any(censMin)) {
    minAppend <- ifelse(censMin,"+"," ")
  } else {
    minAppend <- NULL
  }
  printRestriction <- F
  
  vrnc <- format(x[[4]])
  rnd <- cmptRoundDigits(as.vector(x[[4]]^2),sigfigs)
  append <- rAppend
  vrnc[1:length(vrnc)] <- removeLeadingTrailing (frmtCol(as.vector(x[[4]]^2), rnd, sigfigs, nonsci.limit, 1, append))
  if (isDate) {
    xformCol <- c(3,5:(length(x)-3))
    for (j in 1:length(xformCol)) {
      if (substring(names(x)[xformCol[j]],1,2)=="Pr") xformCol[j] <- NA
    }
    xformCol <- xformCol[!is.na(xformCol)]
    orgn <- "1970-01-01"
  }
  for (i in 1:(length(x)-2)) {
    frmtTmp <- format(x[[i]])
    rnd <- cmptRoundDigits(as.vector(x[[i]]),sigfigs)
    if (i %in% c(3:5)) {
      append <- rAppend
    } else if (i==6) {
      append <- minAppend
    } else if (names(x)[i]==" Max") {
      append <- maxAppend
    } else append <- NULL
    if (isDate) {
      if (i %in% xformCol) {
        frmtTmp[1:length(frmtTmp)] <- format(as.Date(x[[i]],orgn))
      } else frmtTmp[1:length(frmtTmp)] <- removeLeadingTrailing (frmtCol(as.vector(x[[i]]), rnd, sigfigs, nonsci.limit, 1, append))
    } else frmtTmp[1:length(frmtTmp)] <- removeLeadingTrailing (frmtCol(as.vector(x[[i]]), rnd, sigfigs, nonsci.limit, 1, append))
    x[[i]] <- frmtTmp
  }
  append <- "%"
  frmtTmp <- format(rowPct)
  rnd <- cmptRoundDigits(as.vector(rowPct),sigfigs)
  frmtTmp[1:length(frmtTmp)] <- removeLeadingTrailing (frmtCol(as.vector(rowPct), rnd, sigfigs, nonsci.limit, 1, append))
  rowPct <- frmtTmp
  frmtTmp <- format(colPct)
  rnd <- cmptRoundDigits(as.vector(colPct),sigfigs)
  frmtTmp[1:length(frmtTmp)] <- removeLeadingTrailing (frmtCol(as.vector(colPct), rnd, sigfigs, nonsci.limit, 1, append))
  colPct <- frmtTmp
  frmtTmp <- format(totPct)
  rnd <- cmptRoundDigits(as.vector(totPct),sigfigs)
  frmtTmp[1:length(frmtTmp)] <- removeLeadingTrailing (frmtCol(as.vector(totPct), rnd, sigfigs, nonsci.limit, 1, append))
  totPct <- frmtTmp
  
  if(print.it & printer) {
    cat("Tabled descriptive statistics by strata\nCall:\n     ",format(attr(x,"call")),"\n")
    cat("            - NaN denotes strata with no observations\n")
    cat("            - NA arises from missing or censored data\n")
  }
  statTable <- NULL
  specialFormat <- NULL
  template <- NULL
  term <- 0
  pterm <- 0
  qterm <- 0
  censoredExtrema <- F
  if (length(stat)==1) {
    if (is.na(pmatch(stat,stat.chc))) {
      z <- parseFormat (stat)
      if (length(z$stat) > 0) {
        specialFormat <- T
        stat <- z$stat
        specialFormat <- c(z$specialFormat,"")
      }
    }
  }
  
  for (s in stat) {
    indx <- pmatch(s,stat.chc)
    if (is.na(indx)) {
      warning(paste(s,"is unrecognized stat"))
    } else if (indx < 16 && any(is.na(xindx[[indx]]))) {
      warning(paste("descriptives needed for",s,"are unavailable"))
    } else {
      if (is.null(specialFormat)) {
        if(print.it & printer) cat("\n##### ",Labels[indx],ifelse1(na.rm," nonmissing "," "),"observations within strata\n")
        if (indx %in% c(3,4,6,12) & !is.null(rAppend)) {
          printRestriction <- T
          if(print.it & printer) cat("            - (+ denotes a restricted mean-- see output for restriction on observation time)\n")
        } else if (indx %in% c(8,9,13) & !(is.null(minAppend) & is.null(maxAppend))) {
          if (print.it & printer) cat("            - (+ denotes a censored estimate of extrema)\n")
        }
        if (print.it & printer) cat("\n")
        if (indx %in% c(1:6,8,9)) {
          if (print.it & printer) print(x[[xindx[[indx]][1]]],quote=F)
          statTable <- c(statTable,list(x[[xindx[[indx]][1]]]))
        } else if (indx==7) {
          if (print.it & printer) print(vrnc,quote=F)
          statTable <- c(statTable,list(vrnc))
        } else if (indx==16) {
          if (print.it & printer) print(rowPct,quote=F)
          statTable <- c(statTable,list(rowPct))
        } else if (indx==17) {
          if (print.it & printer) print(colPct,quote=F)
          statTable <- c(statTable,list(colPct))
        } else if (indx==18) {
          if (print.it & printer) print(totPct,quote=F)
          statTable <- c(statTable,list(totPct))
        } else if (indx %in% 10:11) {
          statTable <- c(statTable,x[xindx[[indx]]])
          if (print.it & printer) {
            for (j in xindx[[indx]]) {
              cat("#",c("Quantile: ","Probability: ")[indx-9],names(x)[j],"\n")
              print(x[[j]],quote=F)
            }
          }
        } else if (indx==12) {
          tmp <- x[[3]]
          tmp[1:length(tmp)] <- paste(as.vector(tmp)," (",as.vector(x[[4]]),")",sep="")
          if (print.it & printer) print(tmp,quote=F)
          statTable <- c(statTable,list(tmp))
        } else {
          tmp <- x[[xindx[[indx]][1]]]
          tmp[1:length(tmp)] <- paste("(",as.vector(tmp),",",as.vector(x[[xindx[[indx]][2]]]),")",sep="")
          if (print.it & printer) print(tmp,quote=F)
          statTable <- c(statTable,list(tmp))
        }
      } else {
        if (indx %in% c(3,4,6,12) & !is.null(rAppend)) {
          printRestriction <- T
        } else if (indx %in% c(8,9,13) & !(is.null(minAppend) & is.null(maxAppend))) {
          censoredExtrema <- T
        }
        if (indx %in% c(1:6,8,9)) {
          tmp <- x[[xindx[[indx]][1]]]
          sL <- specialLabel[[indx]][1]
        } else if (indx==7) {
          tmp <- vrnc
          sL <- specialLabel[[indx]][1]
        } else if (indx==16) {
          tmp <- rowPct
          sL <- specialLabel[[indx]][1]
        } else if (indx==17) {
          tmp <- colPct
          sL <- specialLabel[[indx]][1]
        } else if (indx==18) {
          tmp <- totPct
          sL <- specialLabel[[indx]][1]
        } else if (indx == 10) {
          if (qterm < length(xindx[[10]])) qterm <- qterm + 1
          tmp <- x[[xindx[[10]][qterm]]]
          sL <- specialLabel[[indx]][qterm]
        } else if (indx == 11) {
          if (pterm < length(xindx[[11]])) pterm <- pterm + 1
          tmp <- x[[xindx[[11]][pterm]]]
          sL <- specialLabel[[indx]][pterm]
        } else if (indx==12) {
          tmp <- x[[3]]
          tmp[1:length(tmp)] <- paste(as.vector(tmp)," (",as.vector(x[[4]]),")",sep="")
          sL <- specialLabel[[indx]][1]
        } else {
          tmp <- x[[xindx[[indx]][1]]]
          tmp[1:length(tmp)] <- paste("(",as.vector(tmp),",",as.vector(x[[xindx[[indx]][2]]]),")",sep="")
          sL <- specialLabel[[indx]][1]
        }
        term <- term + 1
        if (term == 1) {
          tmp[1:length(tmp)] <- paste(specialFormat[term],as.vector(tmp),sep="")
          statTable <- tmp
          template <- paste(specialFormat[term],sL,sep="")
        } else {
          statTable[1:length(statTable)] <- paste(as.vector(statTable),specialFormat[term],as.vector(tmp),sep="")
          template <- paste(template,specialFormat[term],sL,sep="")
        }
      }
    }
  }
  if (!is.null(specialFormat)) {
    term <- term + 1
    statTable[1:length(statTable)] <- paste(as.vector(statTable),specialFormat[term],sep="")
    template <- paste(template,specialFormat[term],sep="")
    if (print.it & printer) {
      if (printRestriction || censoredExtrema)
        cat("            - (+ denotes a censored estimate-- see output for restriction on observation time)\n")
      cat("\nFormat: ",template,"\n\n")
      if (attr(x,"marginOnly")) {
        print(statTable[-1], quote=F)
        if (printRestriction) {
          cat("\n##### Restriction on observations within strata for computing means\n")
          print(x[[length(x)-3]][-1],quote=F)
        }
      } else {
        print(statTable, quote=F)
        if (printRestriction) {
          cat("\n##### Restriction on observations within strata for computing means\n")
          print(x[[length(x)-3]],quote=F)
        }
      }
    }
  } else {
    if (printRestriction) {
      if(print.it & printer) {
        cat("\n##### Restriction on observations within strata for computing means\n")
        cat("            - NaN denotes strata with no observations\n\n")
        print(x[[length(x)-3]],quote=F)
      }
      statTable <- c(statTable,list(x[[length(x)-2]]))
    }
    names(statTable) <- c(stat,ifelse1(printRestriction,"Restriction",NULL))
  }
  invisible(statTable)
}
