binomInference.agresti <-
function(y, n=length(y), null.hypothesis=NA, test.type="two.sided", conf.level=0.95, version=F) {
  
  vrsn <- "20141002"
  if (version) return(vrsn)
  if(length(y) > 1){
    if(length(y) != n){
      stop("n must be equal to the length of y")
    }
  }
  y <- sum(y)
  if(y < 0 | y > n) stop("y must be an integer between 0 and n inclusive")
  alpha <- (1 - conf.level) / 2
  y <- y + qnorm(alpha)^2 / 2
  n <- n + qnorm(alpha)^2
  phat <- y / n
  SE <- sqrt(phat * (1 - phat) / n)
  wlo <- phat + qnorm(alpha)*sqrt(phat * (1-phat) / n)
  whi <- phat - qnorm(alpha)*sqrt(phat * (1-phat) / n)
  if(!is.na(null.hypothesis)) {
    Z <- (phat - null.hypothesis) / SE
    plo <- pnorm(Z)
    phi <- 1 - pnorm(Z)
    p2 <- 2 * min(plo,phi)
    if(test.type=="less") p.value <- plo
    else if(test.type=="greater") p.value <- phi
    else p.value <- p2
  } else Z <- p.value <- NA
  rslt <- c(n, phat, SE, Z, wlo, whi, p.value)
  names(rslt) <- c("n", "Est", "SE", "Statistic", "CIlo", "CIhi", "p.value")
  attr(rslt,"method") = "Agresti"
  rslt
}
