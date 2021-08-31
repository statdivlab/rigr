# Print method for class "ttest"
#' @noRd
#' @export
print.ttest <-
  function(x, ...) {
    out <- x
    cat("\nCall:\n")
    print(out$call)
    main <- out$tab
    main2 <- out$geo
    dfr <- out$df
    pval <- out$p
    tstat <- out$tstat
    var1 <- out$var1
    var2 <- out$var2
    by <- out$by
    par <- out$par
    geom = as.logical(par[1])
    null.hypoth = par[2]
    if (geom) {
      null.hypoth = exp(as.numeric(null.hypoth))
    }
    alternative = par[3]
    var.eq = as.logical(par[4])
    conf.level = par[5]
    matched = as.logical(par[6])
    digits = par[7]
    null.hypoth <- as.numeric(format(null.hypoth, digits = digits))
    if (var.eq) {robust <- FALSE}
    if (!var.eq) {robust <- TRUE}
    myargs <- c(deparse(substitute(var1)), deparse(substitute(var2)), deparse(substitute(by)))
    if (alternative == "less") {par1 <- c(">=", "<", "Pr(T < t) = ")}
    if (alternative == "two.sided") {par1 <- c("=", "!=", "Pr(|T| > t) = ")}
    if (alternative == "greater") {par1 <- c("<=",">", "Pr(T > t) = ")}
    par2 <- ""
    partest <- ""
    partest2 <- "difference in"
    if (geom) {partest = c("geometric")}
    if (geom) {partest2 <- "ratio of"}
    
    if (length(var2) > 1 & !is.na(var2[1]) & matched == FALSE) {
      par2 <- "presuming equal variances"; if (robust == TRUE) {par2 <- "allowing for unequal variances"}
    }
    if (length(by) == 1 & is.na(by[1])) { #If not given a strata variable
      if (length(var2) == 1 & is.na(var2[1])) { #If additionally not given second variable
        inference0 <- paste("Ho: ", partest, " mean ", par1[1], " ", null.hypoth," ;", sep = "")
        inference1 <- paste("Ha: ", partest, " mean ", par1[2], " ", null.hypoth, sep = "")
        inferencet <- paste("t = ", tstat, sep = "")
        inferencedf <- paste("df = ",dfr, sep = "")
        inferencep <- paste(par1[3],pval)
        cat("\nOne-sample t-test", ":\n", collapse = "\n")
        if (!geom) {
          cat("Summary:\n")
          print(main)
        } else {
          cat("Summary:\n")
          print(main2)
        } 
        cat("\n",inference0,"\n", inference1,"\n", inferencet,",", inferencedf,"\n",inferencep, collapse = "\n")
      }
      if (length(var2) > 1) { #If given a second variable
        #dfr <- as.numeric(format(dfr[3], digits = digits))
        inference0 <- paste("Ho: ", partest2, " ", partest, " means ", par1[1], " ", null.hypoth," ;", sep = "")
        inference1 <- paste("Ha: ", partest2, " ", partest, " means ", par1[2], " ", null.hypoth, sep = "")
        inferencet <- paste("t = ", tstat, sep = "")
        inferencedf <- paste("df = ",dfr, sep = "")
        inferencep <- paste(par1[3],pval)
        if (!matched) {cat("\nTwo-sample t-test", par2, ":\n", collapse = "\n")}
        if (matched) {cat("\nTwo-sample (matched) t-test", par2, ":\n", collapse = "\n")}
        if (!geom) {
          cat("Summary:\n")
          print(main)
        } else {
          cat("Summary:\n")
          print(main2)
        }
        cat("\n",inference0,"\n", inference1,"\n", inferencet,",", inferencedf,"\n",inferencep, collapse = "\n")
      }
    }
    
    if (length(by) > 1) { #If given a strata variable
      
      if (length(var2) > 1) { #If given a second variable
        inference0 <- paste("Ho: ", partest2, " ", partest, " means ", par1[1], " ", null.hypoth," ;", sep = "")
        inference1 <- paste("Ha: ", partest2, " ", partest, " means ", par1[2], " ", null.hypoth, sep = "")
        inferencet <- paste("t = ", tstat, sep = "")
        inferencedf <- paste("df = ",dfr, sep = "")
        inferencep <- paste(par1[3],pval)
        if (!matched) {cat("\nTwo-sample t-test", par2, ":\n", collapse = "\n")}
        if (matched) {cat("\nTwo-sample (matched) t-test", par2, ":\n", collapse = "\n")}
        if (!geom) {
          cat("Summary:\n")
          print(main)
        } else {
          cat("Summary:\n")
          print(main2)
        } 
        cat("\n",inference0,"\n", inference1,"\n", inferencet,",", inferencedf,"\n",inferencep, collapse = "\n")
      }
    }
  }