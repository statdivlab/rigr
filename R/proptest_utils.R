# Print method for class "proptest"
#' @noRd
#' @export
print.proptest <- function(x, ...) {
    out <- x
    cat("\nCall:\n")
    print(out$call)
    main <- out$tab
    pval <- out$pval
    zstat <- out$zstat
    var1 <- out$var1
    var2 <- out$var2
    by <- out$by
    par <- out$par
    null.hypoth = par[1]
    test.type = par[2]
    conf.level = par[3]
    exact = as.logical(par[4])
    digits = par[6]
    null.hypoth <- as.numeric(format(null.hypoth, digits = digits))
    myargs <- c(deparse(substitute(var1)), deparse(substitute(var2)), deparse(substitute(by)))
    if (test.type == "less") {par1 <- c(">=", "<")}
    if (test.type == "two.sided") {par1 <- c("=", "!=")}
    if (test.type == "greater") {par1 <- c("<=",">")}
    par2 <- ""
    partest <- ""
    partest2 <- "difference in"
    
    if (length(by) == 0 && is.null(by[1]) && length(var2) == 0 && is.null(var2[1])) { #one sample
      propInf0 <- paste("Ho: True proportion is ", par1[1], " ", null.hypoth, ";", sep="")
      propInf1 <- paste("Ha: True proportion is ", par1[2], " ", null.hypoth, sep="")
      propZ <- ifelse(!exact, paste("Z = ", zstat, sep=""), "")
      propp <- paste("p-value = ", pval, sep="")
      if (exact){
        cat("\nOne-sample proportion test (exact)", ":\n", collapse = "\n")
      } else {
        cat("\nOne-sample proportion test (approximate)", ":\n", collapse = "\n")
      }
      ### add in number of success and number of trials
      print(main)
      cat("Summary:\n")
      cat("\n",paste(propInf0),
          "\n", paste(propInf1),
          "\n", propZ,
          "\n",propp, collapse = "\n")
    } else{ # two sample
      propInf0 <- paste("Ho: Difference in proportions ", par1[1], " ", null.hypoth, sep = "")
      propInf1 <- paste("Ha: Difference in proportions ", par1[2], " ", null.hypoth, sep = "")
      # propInf0 <- paste("Ho: ", partest2, " ", partest, " proportions ", par1[1], " ", 
      #                   null.hypoth," ;", sep = "")
      # propInf1 <- paste("Ha: ", partest2, " ", partest, " proportions ", par1[2], " ", 
      #                   null.hypoth, sep = "")
      propZ <- ifelse(!exact, paste("Z = ", zstat, sep=""), "")
      propp <- paste("p.value = ", pval, sep="")
      cat("\nTwo-sample proportion test (approximate)", ":\n", collapse = "\n")
      print(main)
      cat("Summary:\n")
      cat("\n",paste(propInf0),
          "\n", paste(propInf1),
          "\n", propZ,
          "\n",propp, collapse = "\n")
    }
  }