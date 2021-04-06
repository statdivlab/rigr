## A function to pring uRegress objects
## Args: x            - the uRegress object
##       augmented    - whether or not the object has an augmented coefficients matrix
##       digits       - the number of digits to round to
##       signif.stars - whether to print stars for significant results
##       suppress     - whether or not to suppress printing of the raw coefficients (for example, in logistic regression)
##       version      - returns only the version of the function
## Returns: a printed uRegress object
## Version: 2015 05 31
print.uRegress <-
  function (x,...,augmented=TRUE,digits=max(3,getOption("digits")-3),signif.stars=FALSE, suppress, version=FALSE) {
    
    suppress <- x$suppress
    vrsn <- "20150525"
    
    
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
    if(!is.null(dim(x$model))){
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
    if (version) return(vrsn)
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
    f.PH <- function (x, digits = max(getOption("digits") - 3, 3), signif.stars = getOption("show.signif.stars"), suppress, ...) {
      
      if (!is.null(x$call)) {
        cat("Call:\n")
        dput(x$call)
        cat("\n")
      }
      if (!is.null(x$fail)) {
        cat(" Coxreg failed.", x$fail, "\n")
        return()
      }
      savedig <- options(digits = digits)
      on.exit(options(savedig))
      omit <- x$na.action
      cat("  n=", x$n)
      if (!is.null(x$nevent)) 
        cat(", number of events=", x$nevent, "\n")
      else cat("\n")
      if (length(omit)) 
        cat("   (", naprint(omit), ")\n", sep = "")
      if (nrow(x$coefficients) == 0) {
        cat("   Null model\n")
        return()
      }
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
      } else {
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
      
      ## for each one, print the correct stuff
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
      cat("\n")
      if (!is.null(x$concordance)) {
        cat("Concordance=", format(round(x$concordance[1], 3)), 
            " (se =", format(round(x$concordance[2], 3)), ")\n")
      }
      cat("Rsquare=", format(round(x$rsq["rsq"], 3)), "  (max possible=", 
          format(round(x$rsq["maxrsq"], 3)), ")\n")
      cat("Likelihood ratio test= ", format(round(x$logtest["test"], 
                                                  2)), "  on ", x$logtest["df"], " df,", "   p=", format(x$logtest["pvalue"]), 
          "\n", sep = "")
      cat("Wald test            = ", format(round(x$waldtest["test"], 
                                                  2)), "  on ", x$waldtest["df"], " df,", "   p=", format(x$waldtest["pvalue"]), 
          "\n", sep = "")
      cat("Score (logrank) test = ", format(round(x$sctest["test"], 
                                                  2)), "  on ", x$sctest["df"], " df,", "   p=", format(x$sctest["pvalue"]), 
          sep = "")
      if (is.null(x$robscore)) 
        cat("\n\n")
      else cat(",   Robust = ", format(round(x$robscore["test"], 
                                             2)), "  p=", format(x$robscore["pvalue"]), "\n\n", sep = "")
      if (x$used.robust) 
        cat("  (Note: the likelihood ratio and score tests", 
            "assume independence of\n     observations within a cluster,", 
            "the Wald and robust score tests do not).\n")
      invisible()
    }
    printerGEE <- function(x, digits = max(getOption("digits") - 3, 3), signif.stars = getOption("show.signif.stars"), suppress, ...){
      if(!is.null(x$augmented)){
        augmented <- x$augmented
      } else{
        augmented <- FALSE
      }
      cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
          "\n\n", sep = "")
      cat("Deviance Residuals: \n")
      if (length(x$deviance.resid) > 5) {
        x$deviance.resid <- setNames(quantile(x$deviance.resid, 
                                              na.rm = TRUE), c("Min", "1Q", "Median", "3Q", "Max"))
      }
      xx <- zapsmall(x$deviance.resid, digits + 1L)
      print.default(xx, digits = digits, na.print = "", print.gap = 2L)
      if (length(x$aliased) == 0L) {
        cat("\nNo Coefficients\n")
      }else {
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
      ## for each one, print the correct stuff
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
      cat("\n Estimated Scale Parameters: \n")
      print(format(x$dispersion))
      
      cat("\n Correlation: Structure = ", x$corstr, "\n")
      cat("\n Number of Clusters: ", length(x$clusz), "\n")
      cat("\n Maximum Cluster Size: ", max(x$clusz), "\n")
      cat("\n")
      invisible(x)
    }
    x$augmented <- augmented
    x$suppress <- suppress
    #	cat("n =",x$n)
    if(!is.null(x$na.action)) {
      cat("(",length(x$na.action)," cases deleted due to missing values)")
      cat("\n\n")
    }
    if(!x$anyRepeated | x$fnctl =="hazard"){
      if (x$fnctl %in% c("mean","geometric mean")) {
        #f <- getAnywhere(print.summary.lm)$objs[[1]]
        f <- printerLm
      } else if (x$fnctl == "hazard") {
        f <- f.PH
      } else f <- printerGlm
    } else {
      f <- printerGEE
    }
    if (augmented) {
      x$coefficients <- x$augCoefficients
      x$conf.int <- NULL
    }
    f(x,digits=digits,signif.stars=signif.stars, suppress=suppress)
  }
