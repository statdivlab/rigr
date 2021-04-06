print.ttest <-
function(x, ...) {
    out <- x
    cat("\nCall:\n")
    print(out$call)
    main <- out$tab
    main2 <- out$geo
    main3 <- out$wald
    main4 <- out$exact
    chisqTest <- out$chisqTest
    prop <- chisqTest[1]
    exact <- chisqTest[2]
    chisqStat <- chisqTest[3]
    chisqdf <- chisqTest[4]
    chisqP <- chisqTest[5]
    dfr <- out$df
    pval <- out$p
    tstat <- out$tstat
    var1 <- out$var1
    var2 <- out$var2
    by <- out$by
    par <- out$par
    geom = par[1]
    null.hypoth = par[2]
    if (geom == T) {null.hypoth = exp(as.numeric(null.hypoth))}
    test.type = par[3]
    var.eq = par[4]
    conf.level = par[5]
    matched = par[6]
    digits = par[7]
    null.hypoth <- as.numeric(format(null.hypoth, digits = digits))
    if (var.eq == T) {robust <- F}
    if (var.eq == F) {robust <- T}
    myargs <- c(deparse(substitute(var1)), deparse(substitute(var2)), deparse(substitute(by)))
    if (test.type == "less") {par1 <- c(">=", "<", "Pr(T < t) = ")}
	if (test.type == "two.sided") {par1 <- c("=", "!=", "Pr(|T| > t) = ")}
	if (test.type == "greater") {par1 <- c("<=",">", "Pr(T > t) = ")}
	par2 <- ""
    partest <- ""
    partest2 <- "difference in"
    if (geom == T) {partest = c("geometric")}
    if (geom == T) {partest2 <- "ratio of"}
    
    if (length(var2) > 1 & !is.na(var2[1]) & matched == F) {
        par2 <- "presuming equal variances"; if (robust == T) {par2 <- "allowing for unequal variances"}
	}
    if (length(by) == 1 & is.na(by[1])) { #If not given a strata variable
    if (length(var2) == 1 & is.na(var2[1])) { #If additionally not given second variable
        inference0 <- paste("Ho: ", partest, " mean ", par1[1], " ", null.hypoth," ;", sep = "")
        inference1 <- paste("Ha: ", partest, " mean ", par1[2], " ", null.hypoth, sep = "")
        inferencet <- paste("t = ", tstat, sep = "")
        inferencedf <- paste("df = ",dfr, sep = "")
        inferencep <- paste(par1[3],pval)
        propInf0 <- paste("Ho: True proportion is ", null.hypoth, ";", sep="")
        propInf1 <- paste("Ha: True proportion is not ", null.hypoth, sep="")
        propChi <- paste("Chi-squared = ", chisqStat, sep="")
        propdf <- paste("df = ", chisqdf, sep="")
        propp <- paste("p.value = ", chisqP, sep="")
        cat("\nOne-sample t-test", ":\n", collapse = "\n")
        if (geom == F & prop == F) {cat("Summary:\n")
            print(main)}
        if (geom == T & prop == F) {
            cat("Summary:\n")
            print(main2)
        } 
        if (geom == F & prop == T){
          cat("Summary:\n")
          if(exact == T){
            print(main4)
            propChi <- paste("# of successes = ", chisqStat, sep="")
            propdf <- paste("# of trials = ", chisqdf, sep="")
          } else {
            print(main3)
          }
        }
        if(prop == F){
          cat("\n",inference0,"\n", inference1,"\n", inferencet,",", inferencedf,"\n",inferencep, collapse = "\n")
        } else {
          cat("\n",paste(inference0, "                 ", propInf0, sep=""),"\n", paste(inference1, "                  ", propInf1, sep=""),"\n", inferencet,",", inferencedf, "          ",propChi, propdf ,"\n",inferencep, "    ", propp, collapse = "\n")
        }
    }
    if (length(var2) > 1) { #If given a second variable
        #dfr <- as.numeric(format(dfr[3], digits = digits))
        inference0 <- paste("Ho: ", partest2, " ", partest, " means ", par1[1], " ", null.hypoth," ;", sep = "")
        inference1 <- paste("Ha: ", partest2, " ", partest, " means ", par1[2], " ", null.hypoth, sep = "")
        inferencet <- paste("t = ", tstat, sep = "")
        inferencedf <- paste("df = ",dfr, sep = "")
        inferencep <- paste(par1[3],pval)
        if (matched == F) {cat("\nTwo-sample t-test", par2, ":\n", collapse = "\n")}
        if (matched == T) {cat("\nTwo-sample (matched) t-test", par2, ":\n", collapse = "\n")}
        propInf0 <- paste("Ho: True proportion is ", null.hypoth, ";", sep="")
        propInf1 <- paste("Ha: True proportion is not ", null.hypoth, sep="")
        propChi <- paste("Chi-squared = ", chisqStat, sep="")
        propdf <- paste("df = ", chisqdf, sep="")
        propp <- paste("p.value = ", chisqP, sep="")
        cat("\nOne-sample t-test", ":\n", collapse = "\n")
        if (geom == F & prop == F) {cat("Summary:\n")
                                    print(main)}
        if (geom == T & prop == F) {
          cat("Summary:\n")
          print(main2)
        } 
        if (geom == F & prop == T){
          cat("Summary:\n")
          if(exact == T){
            print(main4)
            propChi <- paste("# of successes = ", chisqStat, sep="")
            propdf <- paste("# of trials = ", chisqdf, sep="")
          } else {
            print(main3)
          }
        }
        if(prop == F){
          cat("\n",inference0,"\n", inference1,"\n", inferencet,",", inferencedf,"\n",inferencep, collapse = "\n")
        } else {
          cat("\n",paste(inference0, "  ", propInf0, sep=""),"\n", paste(inference1, "   ", propInf1, sep=""),"\n", inferencet,",", inferencedf, "          ",propChi, propdf ,"\n",inferencep, "       ", propp, collapse = "\n")
        }
    }
}

if (length(by) > 1) { #If given a strata variable
    
    if (length(var2) > 1) { #If given a second variable
        inference0 <- paste("Ho: ", partest2, " ", partest, " means ", par1[1], " ", null.hypoth," ;", sep = "")
        inference1 <- paste("Ha: ", partest2, " ", partest, " means ", par1[2], " ", null.hypoth, sep = "")
        inferencet <- paste("t = ", tstat, sep = "")
        inferencedf <- paste("df = ",dfr, sep = "")
        inferencep <- paste(par1[3],pval)
        if (matched == F) {cat("\nTwo-sample t-test", par2, ":\n", collapse = "\n")}
        if (matched == T) {cat("\nTwo-sample (matched) t-test", par2, ":\n", collapse = "\n")}
        propInf0 <- paste("Ho: True proportion is ", null.hypoth, ";", sep="")
        propInf1 <- paste("Ha: True proportion is not ", null.hypoth, sep="")
        propChi <- paste("Chi-squared = ", chisqStat, sep="")
        propdf <- paste("df = ", chisqdf, sep="")
        propp <- paste("p.value = ", chisqP, sep="")
        #cat("\nOne-sample t-test", ":\n", collapse = "\n")
        if (geom == F & prop == F) {cat("Summary:\n")
                                    print(main)}
        if (geom == T & prop == F) {
          cat("Summary:\n")
          print(main2)
        } 
        if (geom == F & prop == T){
          cat("Summary:\n")
          if(exact == T){
            print(main4)
            propChi <- paste("# of successes = ", chisqStat, sep="")
            propdf <- paste("# of trials = ", chisqdf, sep="")
          } else {
            print(main3)
          }
        }
        if(prop == F){
          cat("\n",inference0,"\n", inference1,"\n", inferencet,",", inferencedf,"\n",inferencep, collapse = "\n")
        } else {
          cat("\n",paste(inference0, "  ", propInf0, sep=""),"\n", paste(inference1, "   ", propInf1, sep=""),"\n", inferencet,",", inferencedf, "          ",propChi, propdf ,"\n",inferencep, "       ", propp, collapse = "\n")
        }
    }
}
}
