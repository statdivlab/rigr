clusterStatsOld <-
function(y, cluster=NULL, stat="count", subset=NULL, x=NULL, ..., version=F) {
  
  vrsn <- "20121015"
  if (version) return(vrsn)
  
  if (!is.null(cluster)) {
    if (is.list(cluster)) {
      for (i in 1:length(cluster)) {
        if (!is.vector(cluster[[i]])) stop("cluster can only be a vector, matrix, or list of vectors")
      }
      n <- length(cluster[[1]])
      if (length(cluster) > 1) {
        for (i in 2:length(cluster)) {
          if (length(cluster[[i]]) != n) stop("all elements in cluster must be same length")
        }
      }
      snms <- names(cluster)
      if (is.null(cluster)) snms <- rep("",length(cluster))
      tmp <- paste(snms[1],format(cluster[[1]]))
      if (length(cluster) > 1) {
        for (i in 2:length(cluster)) tmp <- paste(tmp,snms[i],format(cluster[[i]]))
      }
    } else {
      cluster <- as.matrix(cluster)
      snms <- dimnames(cluster)[[2]]
      if (is.null(snms)) snms <- rep("",dim(cluster)[2])
      tmp <- paste(snms[1],format(cluster[,1]))
      if (dim(cluster)[2] > 1) {
        for (i in 2:(dim(cluster)[2])) tmp <- paste(tmp,snms[i],format(cluster[,i]))
      }
    }
    snms <- cluster
    cluster <- as.integer(factor(tmp))
  }
  if (stat!="slope") {
    zy <- extract.tableStat(tableStat(y, cluster, stat=stat, subset=subset, ...))
    rslt <- zy[cluster]
    if(!is.null(attr(zy,"restriction"))) attr(rslt,"restriction") <- attr(zy,"restriction")[cluster]
  } else {
    if (is.null(x) || length(x) != length(y) || !is.numeric(x)) stop("need numeric x to compute slopes")
    if (inherits(y,"Date") || is.Surv(y)) stop("need numeric y to compute slopes")
    zxy <- extract.tableStat(tableStat(x*y, cluster, stat="mean", subset=subset, ...))
    zx2 <- extract.tableStat(tableStat(x*x, cluster, stat="mean", subset=subset, ...))
    zx <- extract.tableStat(tableStat(x, cluster, stat="mean", subset=subset, ...))
    zy <- extract.tableStat(tableStat(y, cluster, stat="mean", subset=subset, ...))
    rslt <- (zx2 - zx*zx)
    rslt <- ifelse(is.na(rslt) | rslt==0, NA, rslt)
    rslt <- (zxy - zx * zy) / rslt
    rslt <- rslt[cluster]
  }
  names(rslt) <- snms
  attr(rslt,"stat") <- stat
  rslt
}
