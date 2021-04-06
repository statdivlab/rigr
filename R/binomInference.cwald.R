binomInference.cwald <-
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
  phat <- y / n
  wlo <- phat - 1 / (2 * n) + qnorm(alpha)*sqrt(phat * (1-phat) / n)
  whi <- phat + 1 / (2 * n) - qnorm(alpha)*sqrt(phat * (1-phat) / n)
  if(!is.na(null.hypothesis)) {
    SE <- sqrt(phat * (1 - phat) / n)
    plo <- pnorm((phat + 1 / (2 * n) - null.hypothesis) / SE)
    phi <- 1 - pnorm((phat - 1 / (2 * n) - null.hypothesis) / SE)
    p2 <- 2 * min(plo,phi)
    if(test.type=="less") p.value <- plo
    else if(test.type=="greater") p.value <- phi
    else p.value <- p2
  } else SE <- p.value <- NA
  rslt <- c(n, phat, SE, NA, wlo, whi, p.value)
  names(rslt) <- c("n", "Est", "SE", "Statistic", "CIlo", "CIhi", "p.value")
  attr(rslt,"method") = "cWald"
  rslt
}
