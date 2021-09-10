# Print method for class "proptesti"
#' @noRd
#' @export
print.proptesti <- function(x, ...) {
    out <- x
    cat("\nCall:\n")
    print(out$call)
    main <- out$tab
    pval <- out$pval
    zstat <- out$zstat
    par <- out$par
    null.hypoth = par[1]
    test.type = par[2]
    conf.level = par[3]
    exact = as.logical(par[4])
    twosamp <- as.logical(par[5])
    correct <- as.logical(par[6])
    digits = par[7]
    null.hypoth <- as.numeric(format(null.hypoth, digits = digits))

    if (test.type == "less") {par1 <- c(">=", "<")}
    if (test.type == "two.sided") {par1 <- c("=", "!=")}
    if (test.type == "greater") {par1 <- c("<=",">")}
    par2 <- ""
    partest <- ""
    partest2 <- "difference in"
    
    if (correct){
      cor_str <- "with continuity correction"
    } else{
      cor_str <- ""
    }
    
    if (!twosamp) { #one sample
      propInf0 <- paste("Ho: True proportion is ", par1[1], " ", null.hypoth, ";", sep="")
      propInf1 <- paste("Ha: True proportion is ", par1[2], " ", null.hypoth, sep="")
      propZ <- ifelse(!exact, paste("Z = ", zstat, sep=""), "")
      propp <- paste("p.value = ", pval, sep="")
      if (exact){
        cat("\nOne-sample proportion test (exact)", ":\n", collapse = "\n")
      } else {
        cat("\nOne-sample proportion test (approximate)", cor_str, ":\n", collapse = "\n")
      }
      ### add in number of success and number of trials
      print(main, quote = FALSE)
      cat("Summary:\n")
      cat("\n",paste(propInf0),
          "\n", paste(propInf1),
          "\n", propZ,
          "\n",propp, collapse = "\n")
    } else{ # two sample
      propInf0 <- paste("Ho: Difference in proportions ", par1[1], " ", null.hypoth, sep = "")
      propInf1 <- paste("Ha: Difference in proportions ", par1[2], " ", null.hypoth, sep = "")
      propZ <- ifelse(!exact, paste("Z = ", zstat, sep=""), "")
      propp <- paste("p.value = ", pval, sep="")
      cat("\nTwo-sample proportion test (approximate)", cor_str, ":\n", collapse = "\n")
      print(main, quote = FALSE)
      cat("Summary:\n")
      cat("\n",paste(propInf0),
          "\n", paste(propInf1),
          "\n", propZ,
          "\n",propp, collapse = "\n")
    }
  }