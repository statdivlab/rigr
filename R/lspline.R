lspline <-
function (x, knots, lbl=NULL, parameterization="absolute", version=FALSE) {
  
  vrsn <- "20150519"
  if (version)
    return(vrsn)
  findx <- pmatch(parameterization, c("absolute", "change"))
  if(is.na(findx)) stop("You must enter a valid parameterization")
  parameterization <- c("absolute", "change")[findx]
  cl <- match.call()
  nm <- deparse(cl[[2]])
  n <- length(x)
  p <- length(knots) + 1
  if (is.null(lbl)) {
    cl <- match.call()
    lbl <- as.character(cl[[2]])
  }
  if (length(lbl) != p) {
    lbl <- lbl[1]
    lbl <- paste(lbl, ":", c("min", format(knots)), sep = "")
  }
  m <- matrix(x, n, p) - cbind(0, matrix(rep(knots, each = n),
                                         nrow = n))
  mx <- cbind(matrix(rep(c(knots[1], diff(knots)), each = n),
                     n), Inf)
  m[!is.na(m) & m > mx] <- mx[!is.na(m) & m > mx]
  u <- m < 0
  u[1:n] <- F
  m[u] <- 0
  if (parameterization == "change") {
    mm <- matrix(rep(rep(1:0,p),as.vector(rbind(p:1,c((1:(p-1)),0)))),p)
    m <- m %*% mm
  }
  dimnames(m) <- list(rep("", n), lbl)
  attr(m, "transformation") <- "lspline"
  attr(m, "param") <- parameterization
  attr(m, "name") <- paste("lspline(", nm, ")", sep="")
  attr(m, "prnm") <- nm
  attr(m, "knots") <- knots
  attr(m, "original") <- x
  m
}
