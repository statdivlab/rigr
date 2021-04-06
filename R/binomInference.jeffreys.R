binomInference.jeffreys <-
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
  phat <- qbeta(0.5, y+0.5, n-y+0.5)
  if(y==0) elo <-0
  else {
    elo <- qbeta(alpha, y+0.5, n-y+0.5)
  }
  if(y==n) ehi <- 1
  else {
    ehi <- qbeta(1-alpha,y+0.5,n-y+0.5)
  }
  if(!is.na(null.hypothesis)) {
    plo <- pbeta(null.hypothesis, y+0.5, n-y+0.5)
    phi <- 1 - plo
    p2 <- 2 * min(plo,phi)
    if(test.type=="less") p.value <- plo
    else if(test.type=="greater") p.value <- phi
    else p.value <- p2
  } else p.value <- NA
  rslt <- c(n, phat, NA, NA, elo, ehi, p.value)
  names(rslt) <- c("n", "Est", "SE", "Statistic", "CIlo", "CIhi", "p.value")
  attr(rslt,"method") = "Jeffreys"
  rslt
}
