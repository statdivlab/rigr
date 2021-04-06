binomInference.exactTail <-
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
  if(y==0) elo <-0
  else {
    elo <- qbeta(alpha, y, n-y+1)
  }
  if(y==n) ehi <- 1
  else {
    ehi <- qbeta(1-alpha,y+1,n-y)
  }
  if(!is.na(null.hypothesis)) {
    plo <- pbinom(y, n, null.hypothesis)
    phi <- 1 - plo + dbinom(y, n, null.hypothesis)
    cdf <- pbinom(0:n, n, null.hypothesis)
    if (plo < phi) {
      cdf <- 1 - cdf
      p2 <- plo
    } else p2 <- phi
    u <- cdf <= p2 * (1 + 1e-7)
    if(sum(u) > 0) p2 <- min(p2 + max(cdf[u]),1)
    if(test.type=="less") p.value <- plo
    else if(test.type=="greater") p.value <- phi
    else p.value <- p2
  } else p.value <- NA
  rslt <- c(n, y/n, NA, NA, elo, ehi, p.value)
  names(rslt) <- c("n", "Est", "SE", "Statistic", "CIlo", "CIhi", "p.value")
  attr(rslt,"method") = "Exact(tail)"
  rslt
}
