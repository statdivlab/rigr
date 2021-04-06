correlate <-
function (..., strata = NULL, subset = NULL, conf.level = 0.95, 
    use = "pairwise.complete.obs", method = "pearson", stat = "cor", 
    byStratum = TRUE, version = FALSE) 
{
    vrsn <- "20121017"
    if (version) 
        return(vrsn)
    corr <- function(z, na.rm = T, conf.level = 0.95, method = "pearson") {
        if (!is.matrix(z)) 
            stop("corr must take a matrix z")
        p <- dim(z)[2]
        nms <- dimnames(z)[[2]]
        if (method == "spearman") {
            for (j in 1:p) z[, j] <- rank(z[, j], na.last = "keep")
        }
        z1 <- matrix(rep(z, p), ncol = p * p)
        z2 <- z1[, as.vector(t(matrix(1:(p^2), p)))]
        one <- rep(1, dim(z)[1])
        w <- z1 * z2
        if (na.rm) {
            u <- is.na(w)
            z1[u] <- 0
            z2[u] <- 0
            w[u] <- 0
        }
        else u <- matrix(F, dim(w)[1], dim(w)[2])
        n <- as.vector(one %*% (!u))
        nn <- n
        nn[nn <= 2] <- NA
        vx <- one %*% z1^2 - (one %*% z1)^2/nn
        vy <- one %*% z2^2 - (one %*% z2)^2/nn
        cov <- matrix((one %*% w - ((one %*% z1) * (one %*% z2))/n)/sqrt(vx * 
            vy), nrow = p)
        tst <- cov/sqrt((1 - cov^2)/(nn - 2))
        tst[cov == 1 | cov == -1] <- 0
        z <- log((1 + cov)/(1 - cov))/2
        lo <- hi <- p <- rep(NA, length(n))
        if (any(!is.na(nn))) {
            for (m in unique(n[!is.na(nn)])) p[n == m] <- 2 * 
                pt(-abs(tst[n == m]), m - 2)
        }
        if (any(n > 3)) {
            for (m in unique(n[n > 3])) {
                lo[n == m] <- z[n == m] - qnorm(conf.level/2 + 
                  0.5)/sqrt(m - 3)
                hi[n == m] <- z[n == m] + qnorm(conf.level/2 + 
                  0.5)/sqrt(m - 3)
            }
        }
        lo <- (exp(2 * lo) - 1)/(exp(2 * lo) + 1)
        hi <- (exp(2 * hi) - 1)/(exp(2 * hi) + 1)
        n <- matrix(n, dim(cov)[1])
        p <- matrix(p, dim(cov)[1])
        lo <- matrix(lo, dim(cov)[1])
        hi <- matrix(hi, dim(cov)[1])
        diag(lo) <- 1
        diag(hi) <- 1
        lo[n == 0] <- NaN
        hi[n == 0] <- NaN
        dimnames(cov) <- dimnames(n) <- dimnames(tst) <- dimnames(p) <- dimnames(lo) <- dimnames(hi) <- list(nms, 
            nms)
        rslt <- list(cormtx = cov, n = n, t.stat = tst, pval = p, 
            lo = lo, hi = hi)
        names(rslt)[5:6] <- paste(c("lo", "hi"), format(100 * 
            conf.level), "%CI", sep = "")
        rslt
    }
    chc.use <- c("everything", "complete.obs", "pairwise.complete.obs")
    use <- pmatch(use, chc.use)
    if (is.na(use)) 
        stop("unrecognized value for use")
    chc.method <- c("pearson", "spearman")
    method <- pmatch(method, chc.method)
    if (is.na(method)) 
        stop("unrecognized value for method")
    method <- chc.method[method]
    na.rm <- use == 3
    L <- list(...)
    names(L) <- unlist(match.call(expand.dots = F)$...)
    p <- length(L)
    nms <- NULL
    for (i in 1:p) {
        x <- L[[i]]
        if (is.list(x)) {
            if (is.null(names(x))) {
                nms <- c(nms, paste(names(L)[i], ".V", 1:length(x), 
                  sep = ""))
            }
            else nms <- c(nms, names(x))
        }
        else if (is.matrix(x) & !is.Surv(x)) {
            if (is.null(dimnames(x)[[2]])) {
                nms <- c(nms, paste(names(L)[i], ".V", 1:(dim(x)[2]), 
                  sep = ""))
            }
            else nms <- c(nms, dimnames(x)[[2]])
        }
        else nms <- c(nms, names(L)[i])
    }
    nms <- paste(format(nms, justify = "right"), ": ", sep = "")
    if (!is.null(strata)) {
        if (is.list(strata)) {
            for (i in 1:length(strata)) {
                if (!is.vector(strata[[i]])) 
                  stop("strata can only be a vector, matrix, or list of vectors")
            }
            n <- length(strata[[1]])
            if (length(strata) > 1) {
                for (i in 2:length(strata)) {
                  if (length(strata[[i]]) != n) 
                    stop("all elements in strata must be same length")
                }
            }
            snms <- names(strata)
            if (is.null(snms)) 
                snms <- rep("", length(strata))
            tmp <- paste(snms[1], format(strata[[1]]))
            if (length(strata) > 1) {
                for (i in 2:length(strata)) tmp <- paste(tmp, 
                  snms[i], format(strata[[i]]))
            }
        }
        else {
            strata <- as.matrix(strata)
            snms <- dimnames(strata)[[2]]
            if (is.null(snms)) 
                snms <- rep("", dim(strata)[2])
            tmp <- paste(snms[1], format(strata[, 1]))
            if (dim(strata)[2] > 1) {
                for (i in 2:(dim(strata)[2])) tmp <- paste(tmp, 
                  snms[i], format(strata[, i]))
            }
        }
        strata <- tmp
    }
    rslt <- NULL
    X <- NULL
    nV <- 0
    omitVariables <- NULL
    for (i in 1:p) {
        x <- L[[i]]
        if (is.list(x)) {
            names(x) <- nms[nV + (1:length(x))]
            p2 <- length(x)
            for (j in 1:p2) {
                x2 <- x[[j]]
                if (is.matrix(x2) & !is.Surv(x2)) {
                  dimnames(x2) <- list(NULL, nms[nV + (1:(dim(x)[2]))])
                  nV <- nV + dim(x2)[2]
                  if (!is.character(x2) & !is.factor(x2)) {
                    X <- cbind(X, x2)
                  }
                  else {
                    omitVariables <- c(omitVariables, dimnames(x2)[[2]])
                  }
                }
                else if (is.Surv(x2) | is.factor(x2)) {
                  nV <- nV + 1
                  omitVariables <- c(omitVariables, nms[nV])
                }
                else {
                  x2 <- matrix(x2)
                  nV <- nV + 1
                  dimnames(x2) <- list(NULL, nms[nV])
                  X <- cbind(X, x2)
                }
            }
        }
        else if (is.matrix(x) & !is.Surv(x)) {
            dimnames(x) <- list(NULL, nms[nV + (1:(dim(x)[2]))])
            nV <- nV + dim(x)[2]
            if (!is.character(x) & !is.factor(x)) {
                X <- cbind(X, x)
            }
            else {
                omitVariables <- c(omitVariables, dimnames(x)[[2]])
            }
        }
        else if (is.Surv(x) | is.factor(x)) {
            nV <- nV + 1
            omitVariables <- c(omitVariables, nms[nV])
        }
        else {
            x <- matrix(x)
            nV <- nV + 1
            dimnames(x) <- list(NULL, nms[nV])
            X <- cbind(X, x)
        }
    }
    if (is.null(X)) 
        stop("no variables for correlations")
    if (dim(X)[1] == 1) 
        stop("must have more than 1 variable for correlations")
    if (use == 2) {
        deleteCase <- apply(is.na(X), 1, any)
    }
    else deleteCase <- rep(F, dim(X)[1])
    if (!is.null(subset)) {
        if (is.logical(subset)) 
            keepCase <- (1:(dim(X)[1]))[subset & !deleteCase]
        else if (is.integer(subset)) {
            subset <- subset[!is.na(subset)]
            if (length(subset) == 0) 
                stop("improper format for subset")
            if (all(subset > 0)) {
                keepCase <- subset[!(subset %in% (1:(dim(X)[1]))[deleteCase])]
            }
            else if (all(subset < 0)) {
                keepCase <- (1:(dim(X)[1]))[unique(c(subset, 
                  -(1:(dim(X)[1]))[deleteCase]))]
            }
            else stop("improper format for subset")
        }
        else stop("improper format for subset")
    }
    else keepCase <- !deleteCase
    X <- X[keepCase, ]
    strata <- strata[keepCase]
    if (!is.null(strata)) {
        for (s in sort(unique(strata))) {
            indx <- strata == s
            rslt <- c(rslt, list(corr(X[indx, ], na.rm = na.rm, 
                conf.level = conf.level, method = method)))
        }
        names(rslt) <- sort(unique(strata))
    }
    rslt <- c(rslt, list(All = corr(X, na.rm = na.rm, conf.level = conf.level, 
        method = method)))
    attr(rslt, "omitVariables") <- omitVariables
    attr(rslt, "use") <- chc.use[use]
    attr(rslt, "method") <- method
    attr(rslt, "stat") <- stat
    attr(rslt, "byStratum") <- byStratum
    attr(rslt, "conf.level") <- conf.level
    attr(rslt, "call") <- match.call()
    class(rslt) <- "uCorrelate"
    rslt
}
