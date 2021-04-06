print.tabulate <-
function(x,...){
  out <- x
  cat("\nCall:\n")
  print(out$call)
  tab <- out$rslt
  printer <- out$printer
  tests <- out$tests
  dispRatios <- out$dispRatios
  linMods <- out$linMods
  names <- out$names
  stratified <- out$stratified
  percs <- out$stat
  ## Function that takes an array and returns the chi-squared statistics
  ## on the underlying matrices. Assumes that the array comes from the tableStat()
  ## or tabulate() functions, and thus contains row and column totals.
  ## Created by Brian Williamson 15 July 2014
  ## Last edited 29 July 2014
  ## Meant to be run inside of the tabulate function's main method
  ## Args: data - raw data returned by tableStat()
  ##      tests - the statistical tests to run
  ## dispRatios - TRUE/FALSE, determines if odds ratios etc are to be displayed
  ## Returns: an array with all of the calculated statistics and the raw data
  arrayChisquare <- function(data, tests, dispRatios){
    if(!is.array(data)){stop("Data entered must be an array")}
    ## grab the final dimension of the array
    finalDim <- tail(dim(data), n=1)
    nDims <- length(dim(data))
    ## if nDims is 3, we are dealing with multiple matrices, okay
    ## if not, knock off a dimension and run on the smaller before coming back
    if(nDims> 3){
      chisquares <- apply(data, nDims, arrayChisquare, tests=tests, dispRatios=dispRatios)
    } else if (nDims < 3){ # If dimension less than three we must perform appropriate tests
      # get dimensions
      dims <- dim(data)
      if(length(dims) == 2){ # if dimension 2, simply a matrix so run chi squared on it and return
        rows <- dims[1]
        cols <- dims[2]
        observeds <- data[-rows, -cols]
        expecteds <- matrix(rep(NA, (rows-1)*(cols-1)), nrow=rows-1, ncol=cols-1)
        for(i in 1:(rows-1)){
          for(j in 1:(cols-1)){
            expecteds[i,j] <- data[i, cols]*data[rows,j]/data[rows,cols]
          }
        }
        or <- NULL
        rr <- NULL
        orCI <- NULL
        rrCI <- NULL
        if(dispRatios){
          or <- (observeds[2,2]/observeds[1,2])/(observeds[2,1]/observeds[1,1])
          rr <- (observeds[2,2]/(observeds[1,2]+observeds[2,2]))/(observeds[2,1]/(observeds[1,1]+observeds[2,1]))
          orCI <- c(exp(log(or) - 1.96*sqrt(sum(1/observeds))), exp(log(or) + 1.96*sqrt(sum(1/observeds))))
          rrCI <- c(exp(log(rr) - 1.96*sqrt((1/observeds[2,2] + 1/observeds[2,1]) - (1/(observeds[2,2]+observeds[1,2])
                                                                                     +1/(observeds[2,1]+observeds[1,1])))), exp(log(rr) + 1.96*sqrt((1/observeds[2,2] + 1/observeds[2,1]) 
                                                                                                                                                    - (1/(observeds[2,2]+observeds[1,2])+1/(observeds[2,1]+observeds[1,1])))))
        }
        csStatistic <- sum((observeds - expecteds)^2/expecteds)
        lrStatistic <- NULL
        lrp.value <- NULL
        fisherp <- NULL
        df <- (rows-2)*(cols-2)
        if("lrchisq" %in% tests){
          lrStatistic <- 2*sum(observeds*log(observeds/expecteds), na.rm=TRUE)
          lrp.value <- 1-pchisq(lrStatistic, df)
        }
        if("fisher" %in% tests){
          fisherp <- fisher.test(observeds)$p.value
        }
        uWaldStat <- NULL
        uWaldP <- NULL
        if("uWald" %in% tests){
          uWaldTest <- Exact::exact.test(observeds, method="Z-unpooled", to.plot=FALSE)
          uWaldStat <- uWaldTest$test.statistic
          uWaldP <- uWaldTest$p.value
        }
        uScoreStat <- NULL
        uScoreP <- NULL
        if("uScore" %in% tests){
          uScoreTest <- Exact::exact.test(observeds, method="Z-pooled", to.plot=FALSE)
          uScoreStat <- uScoreTest$test.statistic
          uScoreP <- uScoreTest$p.value
        }
        csp.value <- 1-pchisq(csStatistic, df)
        chisquares <- list(data = data, csStatistic = csStatistic, df = df, csp.value = csp.value, 
                           lrStatistic=lrStatistic, lrp.value=lrp.value, fisherp=fisherp, or=or, orCI=orCI, 
                           rr=rr, rrCI=rrCI, mhStat=NULL, mhDf = NULL, mhP = NULL, mhCI = NULL, mhEst = NULL, 
                           uWaldStat=uWaldStat, uWaldP = uWaldP, uScoreStat=uScoreStat, uScoreP=uScoreP)
        invisible(chisquares)
      } else { # if dimension one, run chi-squared on the vector
        newdat <- data[-dims]
        suppressWarnings(cs <- chisq.test(newdat))
        fisherp <- NULL
        if("fisher" %in% tests){
          fisherp <- fisher.test(newdat)$p.value
        }
        chisquares <- list(data = data, csStatistic = cs$statistic, df = cs$parameter, csp.value = cs$p.value, 
                           lrStatistic=NULL, lrp.value=NULL, fisherp=fisherp, or=NULL, orCI=NULL, rr=NULL, rrCI=NULL
                           , mhStat=NULL, mhDf = NULL, mhP = NULL, mhCI = NULL, mhEst = NULL, uWaldStat=NULL, 
                           uWaldP = NULL, uScoreStat=NULL, uScoreP=NULL)
        invisible(chisquares)
      }
    } else { #if dimension 3, run on each individual matrix (the first two dimensions)
      chisquares <- list()
      nms <- unlist(dimnames(data)[nDims])
      for(i in 1:finalDim){
        dims <- apply(data, nDims, dim)[,i]
        rows <- dims[1]
        cols <- dims[2]
        observeds <- data[-rows, -cols, i]
        expecteds <- matrix(rep(NA, (rows-1)*(cols-1)), nrow=rows-1, ncol=cols-1)
        for(j in 1:(rows-1)){
          for(k in 1:(cols-1)){
            expecteds[j,k] <- data[j, cols, i]*data[rows,k,i]/data[rows,cols,i]
          }
        }
        dat <- data[,,i]
        csStatistic <- sum((observeds - expecteds)^2/expecteds)
        lrStatistic <- NULL
        lrp.value <- NULL
        fisherp <- NULL
        or <- NULL
        rr <- NULL
        orCI <- NULL
        rrCI <- NULL
        if(dispRatios){
          or <- (observeds[2,2]/observeds[1,2])/(observeds[2,1]/observeds[1,1])
          rr <- (observeds[2,2]/(observeds[1,2]+observeds[2,2]))/(observeds[2,1]/(observeds[1,1]+observeds[2,1]))
          orCI <- c(exp(log(or) - 1.96*sqrt(sum(1/observeds))), exp(log(or) + 1.96*sqrt(sum(1/observeds))))
          rrCI <- c(exp(log(rr) - 1.96*sqrt((1/observeds[2,2] + 1/observeds[2,1]) - (1/(observeds[2,2]+observeds[1,2])
                                                                                     +1/(observeds[2,1]+observeds[1,1])))), exp(log(rr) + 1.96*sqrt((1/observeds[2,2] + 1/observeds[2,1]) 
                                                                                                                                                    - (1/(observeds[2,2]+observeds[1,2])+1/(observeds[2,1]+observeds[1,1])))))
        }
        df <- (rows-2)*(cols-2)
        if("lrchisq" %in% tests){
          lrStatistic <- 2*sum(observeds*log(observeds/expecteds), na.rm=TRUE)
          lrp.value <- 1-pchisq(lrStatistic, df)
        }
        if("fisher" %in% tests){
          fisherp <- fisher.test(observeds)$p.value
        }
        mhtest <- NULL
        if("mh" %in% tests){
          mhtest <- mantelhaen.test(data[-rows, -cols, -finalDim])
        }
        uWaldStat <- NULL
        uWaldP <- NULL
        if("uWald" %in% tests){
          uWaldTest <- Exact::exact.test(observeds, method="Z-unpooled", to.plot=FALSE)
          uWaldStat <- uWaldTest$test.statistic
          uWaldP <- uWaldTest$p.value
        }
        uScoreStat <- NULL
        uScoreP <- NULL
        if("uScore" %in% tests){
          uScoreTest <- Exact::exact.test(observeds, method="Z-pooled", to.plot=FALSE)
          uScoreStat <- uScoreTest$test.statistic
          uScoreP <- uScoreTest$p.value
        }
        csp.value <- 1-pchisq(csStatistic, df)
        chisquares[[i]] <- list(dat, csStatistic, df, csp.value, lrStatistic, lrp.value, fisherp, or, orCI, rr, rrCI, mhtest$statistic, 
                                mhtest$parameter, mhtest$p.value, mhtest$conf.int, mhtest$estimate, uWaldStat, uWaldP,
                                uScoreStat, uScoreP)
        names(chisquares[[i]]) <- c("data", "csStatistic", "df", "csp.value", "lrStatistic", "lrp.value", "fisherp", "or", "orCI", 
                                    "rr", "rrCI", "mhStat", "mhDf", "mhP", "mhCI", "mhEst", "uWaldStat", "uWaldP",
                                    "uScoreStat", "uScoreP")
      }
      names(chisquares) <- nms
    }
    invisible(chisquares)
  }
  ## Formats the data and chisquared estimates into a reasonable format.
  ## Has each element of the array followed by its chi-squared statistic,
  ## and then the overall chi-squared statistic.
  ## Meant to be run inside of the tabulate function's print method.
  ## Created 16 July 2014 by Brian Williamson
  ## Last modified 28 July 2014
  ## Relies on the plyr package
  
  ## Args: array - the array of data returned by tableStat()
  ##        list - the list returned by arrayChisquare()
  ## Returns: the printed values, in a list
  tabPrintFormat <- function(array, list){
    nDims <- length(dim(array))
    finalDim <- tail(dim(array), n=1)
    
    if(nDims < 3){
      stat <- list$csStatistic
      df <- list$df
      p <- list$csp.value
      lrstat <- list$lrStatistic
      lrp <- list$lrp.value
      fisherp <- list$fisherp
      or <- list$or
      orCI <- list$orCI
      rr <- list$rr
      rrCI <- list$rrCI
      uWaldStat <- list$uWaldStat
      uWaldP <- list$uWaldP
      uScoreStat <- list$uScoreStat
      uScoreP <- list$uScoreP
      #print(array)
      strMat <- matrix(c(" ", format(stat, digits=5), df, " ", format(p, digits=5), " "), nrow=1)
      colnames(strMat) <- c("Point Estimate", "Test Statistic", "df", "95% CI", "p-value", "Warnings")
      rownames(strMat) <- "Chi-squared"
      if(!is.null(lrstat)){
        strMat <- rbind(strMat, matrix(c(" ", format(lrstat, digits=5), df, " ", format(lrp, digits=5), " "), nrow=1))
        rownames(strMat)[dim(strMat)[1]] <- "LR Chi-squared"
        if(is.nan(lrstat)){
          strMat[dim(strMat)[1],6] <- "lr values NaN due to zero counts in some cells" 
        }
      }
      if(!is.null(fisherp)){
        strMat <- rbind(strMat, matrix(c(" ", " ", " ",  " ", format(fisherp, digits=5), " "), nrow=1))
        rownames(strMat)[dim(strMat)[1]] <- "Fisher's Exact Test"
      }
      if(!is.null(or)){
        strMat <- rbind(strMat, matrix(c(format(or, digits=5), " ", " ", paste("[", format(orCI[1], digits=5), ", ", format(orCI[2], digits=5), "]", sep=""), " ", " "), nrow=1))
        rownames(strMat)[dim(strMat)[1]] <- "Odds Ratio"
        strMat <- rbind(strMat, matrix(c(format(rr, digits=5), " ", " ", paste("[", format(rrCI[1], digits=5), ", ", format(rrCI[2], digits=5), "]", sep=""), " ", " "), nrow=1))
        rownames(strMat)[dim(strMat)[1]] <- "Risk Ratio"
      }
      if(!is.null(uWaldStat)){
        strMat <- rbind(strMat, matrix(c(" ", format(uWaldStat, digits=5), " ", " ", format(uWaldP, digits=5), " "), nrow=1))
        rownames(strMat)[dim(strMat)[1]] <- "Unconditional Exact Test (Wald)"
      }
      if(!is.null(uScoreStat)){
        strMat <- rbind(strMat, matrix(c(" ", format(uScoreStat, digits=5), " ", " ", format(uScoreP, digits=5), " "), nrow=1))
        rownames(strMat)[dim(strMat)[1]] <- "Unconditional Exact Test (Score)"
      }
      invisible(list(array=array, str1=dimnames(array), str2=strMat))
    } else if (nDims > 3){
      arraylist <- list()
      for(i in 1:finalDim){
        arraylist[[i]] <- plyr::aaply(array, nDims, tabPrintFormat, list=list[[i]])
      }
      names(arraylist) <- unlist(dimnames(array)[nDims])
      return(arraylist)
    } else {
      nms <- unlist(dimnames(array)[nDims])
      str1s <- list()
      str2s <- list()
      arrays <- list()
      overallCs <- 0
      overallLr <- 0
      overallDf <- 0
      for(i in 1:finalDim){
        if(i == finalDim){
          colTotal <- array[dim(array)[1],,i]
          colTotal <- colTotal[-length(colTotal)]
          rowTotal <- array[,dim(array)[2],i]
          rowTotal <- rowTotal[-length(rowTotal)]
          total <- array[dim(array)[1], dim(array)[2], i]
        }
        arrays[[i]] <- array[,,i]
        stat <- list[[i]]$csStatistic
        df <- list[[i]]$df
        overallCs <- overallCs + stat
        overallDf <- overallDf + df
        p <- list[[i]]$csp.value
        lrstat <- list[[i]]$lrStatistic
        overallLr <- overallLr + lrstat
        lrp <- list[[i]]$lrp.value
        fisherp <- list[[i]]$fisherp
        or <- list[[i]]$or
        orCI <- list[[i]]$orCI
        rr <- list[[i]]$rr
        rrCI <- list[[i]]$rrCI
        mhStat <- list[[i]]$mhStat
        mhDf <- list[[i]]$mhDf
        mhP <- list[[i]]$mhP
        mhCI <- list[[i]]$mhCI
        mhEst <- list[[i]]$mhEst
        uWaldStat <- list[[i]]$uWaldStat
        uWaldP <- list[[i]]$uWaldP
        uScoreStat <- list[[i]]$uScoreStat
        uScoreP <- list[[i]]$uScoreP
        str1s[[i]] <- paste(nms[i], " :", sep="")
        #print(dat)
        str2s[[i]] <- matrix(c(" ", format(stat, digits=5), df, " ", format(p, digits=5), " "), nrow=1)
        colnames(str2s[[i]]) <- c("Point Estimate", "Test Statistic", "df", "95% CI", "p-value", "Warnings")
        rownames(str2s[[i]]) <- "Chi-squared"
        if(!is.null(lrstat)){
          str2s[[i]] <- rbind(str2s[[i]], matrix(c(" ", format(lrstat, digits=5), df, " ", format(lrp, digits=5), " "), nrow=1))
          rownames(str2s[[i]])[dim(str2s[[i]])[1]] <- "LR Chi-squared"
          if(is.nan(lrstat)){
            str2s[[i]][dim(str2s[[i]])[1],6] <- "lr values NaN due to zero counts in some cells" 
          }
        }
        if(!is.null(fisherp)){
          str2s[[i]] <- rbind(str2s[[i]], matrix(c(" ", " ", " ",  " ",format(fisherp, digits=5), " "), nrow=1))
          rownames(str2s[[i]])[dim(str2s[[i]])[1]] <- "Fisher's Exact Test"
        }
        if(!is.null(mhStat)){
          if(!is.null(mhCI)){
            str2s[[i]] <- rbind(str2s[[i]], matrix(c(format(mhEst, digits=5), format(mhStat, digits=5), mhDf, paste("[", format(mhCI[1], digits=3), ",", format(mhCI[2], digits=3), "]", sep=""),format(mhP, digits=5), ""), nrow=1))
            rownames(str2s[[i]])[dim(str2s[[i]])[1]] <-"Mantel-Haenzsel"
          } else {
            str2s[[i]] <- rbind(str2s[[i]], matrix(c("", format(mhStat, digits=5), mhDf, " ", format(mhP, digits=5), "Mantel-Haenszel m2 reported"), nrow=1))
            rownames(str2s[[i]])[dim(str2s[[i]])[1]] <-"Mantel-Haenzsel"        
          }
        }
        if(!is.null(or)){
          str2s[[i]] <- rbind(str2s[[i]], matrix(c(format(or, digits=5), " ", " ", paste("[", format(orCI[1], digits=5), ", ", format(orCI[2], digits=5), "]", sep=""), " ", " "), nrow=1))
          rownames(str2s[[i]])[dim(str2s[[i]])[1]] <- "Odds Ratio"
          str2s[[i]] <- rbind(str2s[[i]], matrix(c(format(rr, digits=5), " ", " ", paste("[", format(rrCI[1], digits=5), ", ", format(rrCI[2], digits=5), "]", sep=""), " ", " "), nrow=1))
          rownames(str2s[[i]])[dim(str2s[[i]])[1]] <- "Risk Ratio"
        }
        if(!is.null(uWaldStat)){
          str2s[[i]] <- rbind(str2s[[i]], matrix(c(" ", format(uWaldStat, digits=5), " ",  " ",format(uWaldP, digits=5), " "), nrow=1))
          rownames(str2s[[i]])[dim(str2s[[i]])[1]] <- "Unconditional Exact Test (Wald)"
        }
        if(!is.null(uScoreStat)){
          str2s[[i]] <- rbind(str2s[[i]], matrix(c(" ", format(uScoreStat, digits=5), " ",  " ",format(uScoreP, digits=5), " "), nrow=1))
          rownames(str2s[[i]])[dim(str2s[[i]])[1]] <- "Unconditional Exact Test (Score)"
        }
      }
      invisible(list(array=arrays, str1=str1s, str2=str2s, overallCs = overallCs, overallLr = overallLr, overallDf = overallDf, rowTotal = rowTotal, colTotal=colTotal, total=total))
    }
  }
  ## Helper function to print an array returned by tabPrintFormat()
  ## Created 18 July 2014 by Brian Williamson
  ## Last edited 8 August 2014
  tabPrinter <- function(formattedArray, dims, stratified, rowTotal, colTotal, total, percs) {
    ##Take in a matrix, reformat data, return
    dataReFormat <- function(matrix, stratified, rowTotal, colTotal, total, dims, percs){
      if(is.numeric(matrix)){
        return(matrix)
      } else {
        for(i in 1:length(matrix)){
          splitList <- strsplit(matrix[i], " ")
          formatSplitList <- suppressWarnings(as.numeric(splitList[[1]]))
          if(length(formatSplitList)==1){
            matrix[i] <- formatSplitList
          } else {
            for(j in 1:length(formatSplitList)){
              if(!is.na(formatSplitList[j])){
                formatSplitList[j] <- format(formatSplitList[j], digits=4)
                splitList[[1]][j] <- formatSplitList[j]
                count <- as.numeric(formatSplitList[j])
                #indx <- j
              }
            }
          }
          #matrix[i] <- splitList[[1]][indx]
          if(length(splitList[[1]]) > 1){
            if(!stratified & dims > 2 & length(grep("%", splitList[[1]])) != 0){
              counter <- 0
              percVec <- strsplit(percs, " ")[[1]]
              rowPerc <- grep("@row%@", percVec)
              colPerc <- grep("@col%@", percVec)
              totPerc <- grep("@tot%@", percVec)
              first <- min(rowPerc, colPerc, totPerc)
              second <- ceiling(median(c(rowPerc, colPerc, totPerc)))
              third <- max(rowPerc, colPerc, totPerc)
              if(first==second & first==third){
                second <- 5
                third <- 5
              }
              for(k in 1:length(splitList[[1]])){
                if(grepl("%", splitList[[1]][k])){
                  #replace with other
                  newRow <- NULL
                  newCol <- NULL
                  newTot <- NULL
                  nRows <- dim(matrix)[1]
                  nCols <- dim(matrix)[2]
                  if(grepl("row%",percs)){
                    if (i %% nRows == 0){
                      newRow <- paste(format(count/total*100, digits=3), "%", sep="")
                    } else {
                      for(m in 1:length(rowTotal)){
                        if(i %% nRows == m){
                          newRow <- paste(format(count/rowTotal[m]*100, digits=3), "%", sep="")
                        } 
                      }
                    }
                  }
                  if(grepl("col%",percs)){
                    if (i %% nCols == 0){
                      newCol <- paste(format(count/total*100, digits=3), "%", sep="")
                    } else {
                      for(m in 1:length(rowTotal)){
                        if(i %% nCols == m){
                          newCol <- paste(format(count/colTotal[m]*100, digits=3), "%", sep="")
                        } 
                      }
                    }
                  }
                  if(is.null(newCol)){
                    newCol <- ""
                  }
                  if(is.null(newRow)){
                    newRow <- ""
                  }
                  if(grepl("tot%",percs)){
                    newTot <- paste(format(count/total*100, digits=3), "%", sep="")
                  }
                  if(length(rowPerc) != 0){
                    if(first == rowPerc & counter == 0){
                      splitList[[1]][k] <- newRow
                      counter <- 1
                    } else if (second == rowPerc & counter == 1){
                      splitList[[1]][k] <- newRow
                      counter <- 2
                    } else if (length(rowPerc) != 0 & length(colPerc) != 0 & length(totPerc) != 0 & third == rowPerc & counter == 2){
                      splitList[[1]][k] <- newRow
                    }
                  }
                  if(length(colPerc) != 0){
                    if (first == colPerc & counter == 0){
                      splitList[[1]][k] <- newCol
                      counter <- 1
                    } else if (second == colPerc & counter == 1){
                      splitList[[1]][k] <- newCol
                      counter <- 2
                    } else if (length(rowPerc) != 0 & length(colPerc) != 0 & length(totPerc) != 0 & third == colPerc & counter == 2){
                      splitList[[1]][k] <- newCol
                    }
                  }
                  if(length(totPerc) != 0){
                    if (first == totPerc & counter == 0){
                      splitList[[1]][k] <- newTot
                      counter <- 1
                    } else if (second == totPerc & counter == 1){
                      splitList[[1]][k] <- newTot
                      counter <- 2
                    } else if (length(rowPerc) != 0 & length(colPerc) != 0 & length(totPerc) != 0 & third == totPerc & counter == 2){
                      splitList[[1]][k] <- newTot
                    }
                  }
                }
              }
            }
            matrix[i] <- splitList[[1]][1]
            for(l in 2:length(splitList[[1]])){
              matrix[i] <- paste(matrix[i], splitList[[1]][l])
            }
          }
        }
        return(matrix)
      }
    }
    if(dims > 4){
      for(i in 1:length(formattedArray)){
        apply(formattedArray[[i]], 1, tabPrinter, dims=(dims-1), stratified=stratified, rowTotal=rowTotal, colTotal=colTotal, total=total, percs=percs)
      }
    }
    else if(dims == 4){
      j <- 1
      for(i in 1:length(formattedArray)){
        cat("\n",paste(names(formattedArray)[i], " :", sep=""),"\n")
        for(k in 1:length(formattedArray[[1]][,1][[1]])){
          cat("\n", formattedArray[[i]][,2][[j]][[k]], "\n")
          newArray <- dataReFormat(formattedArray[[i]][,1][[j]][[k]], stratified, rowTotal, colTotal, total, dims, percs)
          print(newArray, quote=FALSE)
          if(stratified){
            print(formattedArray[[i]][,3][[j]][[k]], quote=FALSE)
          }
        }
        j <- j+1
      }
    } else if (dims == 3) {
      for(i in 1:length(formattedArray[[1]])){
        cat("\n", formattedArray$str1[[i]], "\n")
        newArray <- dataReFormat(formattedArray$array[[i]], stratified, rowTotal, colTotal, total, dims, percs)
        print(newArray, quote=FALSE)
        if(stratified){
          print(formattedArray$str2[[i]], quote=FALSE)
        }
      }
    } else {
      newArray <- dataReFormat(formattedArray$array, stratified, rowTotal, colTotal, total, dims, percs)
      print(newArray, quote=FALSE)
      print(formattedArray$str2, quote=FALSE)
    }
  }
  ## takes in a list of linear models and formats the  results for printing
  linModFormat <- function(list, names, tests){
    if("wald" %in% tests | "lr" %in% tests | "score" %in% tests){
      if("wald" %in% tests){
        cat("\n", "Wald Test", "\n")
        print(anova(list[[length(list)]], test="Chisq"))
      }
      if("lr" %in% tests){
        cat("\n", "Likelihood Ratio Test", "\n")
        print(anova(list[[length(list)]], test="LR"))
      }
      if("score" %in% tests){
        cat("\n", "Rao's Score Test", "\n")
        print(anova(list[[length(list)]], test="Rao"))
      }
      cat("\n", "strataOne = ", names[1], "\n")
    }
  }
  ## Function to print the overall test
  ## calculates and returns
  printOverall <- function(formattedArray, dims){
    
    if(dims==3){
      overallCs <- formattedArray$overallCs
      overallLr <- formattedArray$overallLr
      overallDf <- formattedArray$overallDf
      csp <- 1-pchisq(overallCs, overallDf)
      lrp <- NULL
      if(length(overallLr) != 0){
        lrp <- 1-pchisq(overallLr, overallDf)
      }
      invisible(list(csStat=overallCs, csp=csp, lrStat=overallLr, lrp=lrp, df=overallDf))
    } else if (dims==4){
      overallCs <- 0
      overallLr <- 0
      overallDf <- 0
      for(i in 1:length(formattedArray)){
        if(is.na(formattedArray[[i]][1,4])){
          formattedArray[[i]][1,4] <- 0
          formattedArray[[i]][1,6] <- 0
        }
        overallCs <- overallCs + unlist(formattedArray[[i]][1,4])
        overallLr <- overallLr + unlist(formattedArray[[i]][1,5])
        overallDf <- overallDf + unlist(formattedArray[[i]][1,6])
      }
      csp <- NULL
      lrp <- NULL
      if(!is.na(overallCs)){
        csp <- 1-pchisq(overallCs, overallDf)
      }
      if(length(overallLr) != 0){
        lrp <- 1-pchisq(overallLr, overallDf)
      }
      invisible(list(csStat=overallCs, csp=csp, lrStat=overallLr, lrp=lrp, df=overallDf))
    } else if (dims > 4){
      overallList <- list()
      for(i in 1:length(formattedArray)){
        overallList[[i]] <- apply(formattedArray[[i]], 1, printOverall, dims=(dims-1))
      }
      names(overallList) <- unlist(dimnames(formattedArray)[dims])
      invisible(overallList)
    }
  }
  removeLastDims <- function(array){
    if(length(dim(array)) == 2){
      return(array[-dim(array)[1], -dim(array)[2]])
    } else {
      newArray <- plyr::aaply(array, tail(dim(array), n=1), removeLastDims)
    }
  }
  ## Format and print the values
  if(is.list(tab)){
    array <- arrayChisquare(tab[[1]], tests, dispRatios)
    formattedArray <- tabPrintFormat(printer, array)
    dim <- length(dim(tab[[1]]))
    finalDim <- tail(dim(tab[[1]]), n=1)
    reformatVec <- function(vector){
      splitList <- strsplit(vector, " ")
      newList <- rep(0, length(splitList))
      for(i in 1:length(splitList)){
        vec <- suppressWarnings(as.numeric(splitList[[i]]))
        for(j in 1:length(vec)){
          if(!is.na(vec[j])){
            newList[i] <- vec[j]
          }
        }
      }
      return(newList)
    }
    if(dim > 2){
      rowTotal <- reformatVec(formattedArray$rowTotal)
      colTotal <- reformatVec(formattedArray$colTotal)
      total <- sum(rowTotal)
    } else if(dim == 2){
      rowTotal <- array$data[,dim(array$data)[2]]
      rowTotal <- rowTotal[-length(rowTotal)]
      colTotal <- array$data[dim(array$data)[1],]
      colTotal <- colTotal[-length(colTotal)]
      total <- array$data[dim(array$data)[1], dim(array$data)[2]]
    } else {
      rowTotal <- array$data[length(array$data)]
      colTotal <- array$data[-length(array$data)]
      total <- rowTotal
    }
    tabPrinter(formattedArray, dim, stratified, rowTotal, colTotal, total, percs)
    linModFormat(linMods, names, tests)
    overalls <- printOverall(formattedArray, dim)
  } else {
    array <- arrayChisquare(tab, tests, dispRatios)
    formattedArray <- tabPrintFormat(printer, array)
    dim <- length(dim(tab))
    finalDim <- tail(dim(tab), n=1)
    reformatVec <- function(vector){
      splitList <- strsplit(vector, " ")
      newList <- rep(0, length(splitList))
      for(i in 1:length(splitList)){
        vec <- suppressWarnings(as.numeric(splitList[[i]]))
        for(j in 1:length(vec)){
          if(!is.na(vec[j])){
            newList[i] <- vec[j]
          }
        }
      }
      return(newList)
    }
    if(dim > 2){
      rowTotal <- reformatVec(formattedArray$rowTotal)
      colTotal <- reformatVec(formattedArray$colTotal)
      total <- sum(rowTotal)
    } else if (dim==2){
      rowTotal <- tab[,dim(tab)[2]]
      rowTotal <- rowTotal[-length(rowTotal)]
      colTotal <- tab[dim(tab)[1],]
      colTotal <- colTotal[-length(colTotal)]
      total <- tab[dim(tab)[1], dim(tab)[2]]
    } else{
      rowTotal <- tab[length(tab)]
      colTotal <- tab[-length(tab)]
      total <- rowTotal
    }
    
    tabPrinter(formattedArray, dim, stratified, rowTotal, colTotal, total, percs)
    linModFormat(linMods, names, tests)
    overalls <- printOverall(formattedArray, dim)
  }
  if(dim <= 4){
    overallCs <- overalls$csStat
    overallLr <- overalls$lrStat
    overallDf <- overalls$df
    csp <- overalls$csp
    lrp <- overalls$lrp
  } else if (dim > 4) {
    overallCs <- 0
    overallLr <- 0
    overallDf <- 0
    for(i in 1:length(overalls)){
      overallCs <- overallCs + overalls[[i]][[1]]$csStat
      overallLr <- overallLr + overalls[[i]][[1]]$lrStat
      overallDf <- overallDf + overalls[[i]][[1]]$df
    }
    csp <- 1-pchisq(overallCs, overallDf)
    lrp <- NULL
    if(length(overallLr != 0)){
      lrp <- 1-pchisq(overallLr, overallDf)
    }
  }
  if(!is.null(overallCs)){
    cat("\n", "Overall: ","\n")
    printMat <- matrix(c(format(overallCs, digits=5), format(overallDf, digits=5), format(csp, digits=5)), nrow=1)
    colnames(printMat) <- c("Test Statistic", "df", "p-value")
    rownames(printMat) <- "Chi-squared"
    if(!is.null(lrp)){
      printMat <- rbind(printMat, matrix(c(format(overallLr, digits=5), format(overallDf, digits=5), format(lrp, digits=5)), nrow=1))
      rownames(printMat)[dim(printMat)[1]] <- "Likelihood Ratio"
    }
    print(printMat, quote=FALSE)
  }
}
