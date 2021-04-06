clusterStats <-
function(y, cluster=NULL, stat="count", subset=NULL, x=NULL, ..., version=FALSE) {
  
  vrsn <- "20121024"
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
  uniqueClusters <- unique(cluster)
  nCluster <- length(uniqueClusters)
  rslt <- rep(0,length(cluster))
  restr <- rep(Inf,length(cluster))
  if (stat!="slope") {
    for (cl in uniqueClusters) {
      u <- cluster==cl
      if(length(subset[u]) == 1 && !subset[u]){
        subset[u] <- TRUE
      }
      zy <- extract.tableStat(tableStat(y[u], stat=stat, subset=subset[u], ...))
      if(is.Surv(zy)) {
        if(!is.Surv(rslt)) rslt <- Surv(rslt,rep(1,length(rslt)))
        rslt[u,1] <- zy[,1]
        rslt[u,2] <- zy[,2]
      } else rslt[u] <- zy
      if(!is.null(attr(zy,"restriction"))) restr[u] <- attr(zy,"restriction")
    }
    if(any(restr!=Inf)) attr(rslt,"restriction") <- restr
    if(inherits(y,"Date")) rslt <- as.Date(rslt,origin="1970-01-01")
  } else {
    if (is.null(x) || length(x) != length(y) || !is.numeric(x)) stop("need numeric x to compute slopes")
    if (inherits(y,"Date") || is.Surv(y)) stop("need numeric y to compute slopes")
    for (cl in uniqueClusters) {
      u <- cluster==cl
      zxy <- extract.tableStat(tableStat(x[u]*y[u], stat="mean", subset=subset[u], ...))
      zx2 <- extract.tableStat(tableStat(x[u]^2, stat="mean", subset=subset[u], ...))
      zx <- extract.tableStat(tableStat(x[u], stat="mean", subset=subset[u], ...))
      zy <- extract.tableStat(tableStat(y[u], stat="mean", subset=subset[u], ...))
      tmp <- (zx2 - zx*zx)
      tmp <- ifelse(is.na(tmp) | tmp==0, NA, tmp)
      tmp <- (zxy - zx * zy) / tmp
      rslt[u] <- tmp
    }
  }
  names(rslt) <- snms
  attr(rslt,"stat") <- stat
  rslt
}
