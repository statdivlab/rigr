binomInference.halfP <-
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
  if(y==0) hlo <- 0
  else {
    f <- function(x, n, y, alpha) pbinom(n-y, n, 1-x) - dbinom(n-y, n, 1-x) / 2 - alpha
    hlo <- uniroot(f, c(0,1), n=n, y=y, alpha=alpha, tol=1e-10)$root
  }
  if(y==n) hhi <- 1
  else {
    f <- function (x, n, y, alpha) pbinom(y, n, x) - dbinom(y, n, x) / 2- alpha
    hhi <- uniroot (f, c(0,1), n=n, y=y, alpha=alpha, tol=1e-10)$root
  }
  if(!is.na(null.hypothesis)) {
    plo <- pbinom(y, n, null.hypothesis) - dbinom(y, n, null.hypothesis)/2
    phi <- 1 - plo + dbinom(y, n, null.hypothesis) / 2
    cdf <- pbinom(0:n, n, null.hypothesis) - dbinom(0:n, n, null.hypothesis) / 2
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
  rslt <- c(n, y/n, NA, NA, plo, phi, p.value)
  names(rslt) <- c("n", "Est", "SE", "Statistic", "CIlo", "CIhi", "p.value")
  attr(rslt,"method") = "HalfP"
  rslt
}
