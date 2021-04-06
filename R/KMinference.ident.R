KMinference.ident <-
function (y)
  ciKM <- function (z, obs) {
    k <- sum (z$t <= obs)
    k <- c(z$S[k], z$lower[k], z$upper[k])
    names(k) <- c("Surv Prob", "Lower CI", "Upper CI")
    k
  }
