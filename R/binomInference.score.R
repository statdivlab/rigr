binomInference.score <-
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
  if(y==0) slo <- 0
  else slo <- (2 * n * phat + qnorm(alpha)^2 - 
                 sqrt(4 * n * phat * (1 - phat) * qnorm(alpha)^2 + 
                        qnorm(alpha)^4)) / (2 * (n + qnorm(alpha)^2))
  if(y==n) shi <- 1
  else shi <- (2 * n * phat + qnorm(alpha)^2 +
                 sqrt(4 * n * phat * (1 - phat) * qnorm(alpha)^2 +
                        qnorm(alpha)^4)) / (2 * (n + qnorm(alpha)^2))
  if(!is.na(null.hypothesis)) {
    SE <- sqrt(null.hypothesis * (1 - null.hypothesis) / n)
    Z <- (phat - null.hypothesis) / SE
    plo <- pnorm((phat - null.hypothesis) / SE)
    phi <- 1 - pnorm((phat - null.hypothesis) / SE)
    p2 <- 2 * min(plo,phi)
    if(test.type=="less") p.value <- plo
    else if(test.type=="greater") p.value <- phi
    else p.value <- p2
  } else SE <- Z <- p.value <- NA
  rslt <- c(n, phat, SE, Z, slo, shi, p.value)
  names(rslt) <- c("n", "Est", "SE", "Statistic", "CIlo", "CIhi", "p.value")
  attr(rslt,"method") <- "Score"
  rslt
}
