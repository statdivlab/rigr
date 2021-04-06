bplot<-function (y, x = rep(1, length(y)), data = NA, strata = NULL, 
          xjitter = TRUE, yjitter = TRUE, range = 0, sd = TRUE, sdx = TRUE, 
          log = FALSE, cex = c(meancex = 1, jittercex = 0.8), col = c(sdcol = "dodgerblue3", 
                                                                      jittercol = "gray30"), main = NULL, xlab = NULL, ylab = NULL, 
          names = NULL, ylim = NULL, legend = FALSE) 
{
  if (!is.null(ylim)) {
    if (ylim == "full") {
      ylim <- c(min(y), max(y))
    }
  }
  if (!is.logical(sd)) {
    stop("Variable 'sd' must be logical.")
  }
  if (!is.logical(sdx)) {
    stop("Variable 'sdx' must be logical.")
  }
  if (!is.logical(log)) {
    stop("Variable 'log' must be logical.")
  }
  if (length(cex) == 2) {
    if (!is.numeric(cex[1]) | !is.numeric(cex[2])) {
      stop("Variable 'cex' must have two numeric values.")
    }
  }
  if (length(cex) > 2) {
    warning("Variable 'cex' was supplied more than two values; using the first two.")
  }
  if (length(cex) < 2) {
    stop("Variable 'cex' must have two numeric values.")
  }
  if (length(col) > 2) {
    warning("Variable 'col' was supplied more than two values; using the first two.")
  }
  if (length(col) < 2) {
    stop("Variable 'col' must have two numeric values.")
  }
  if (is.null(x)) {
    x <- rep(0, length(y))
  }
  cl <- match.call()
  if (is.null(names) & length(cl) >= 3) {
    names <- c(deparse(cl[[2]]), deparse(cl[[3]]))
    names(y) <- cl[[2]]
    names(x) <- cl[[3]]
  }
  stratanm <- TRUE
  if (is.null(strata)) {
    strata <- rep(1, length(x))
    stratanm <- FALSE
  }
  if (!is.null(strata)) {
    if (is.list(strata)) {
      for (i in 1:length(strata)) {
        if (!is.vector(strata[[i]])) 
          stop("strata can only be a vector, matrix, or list of vectors")
      }
      n <- length(strata[[1]])
      if (length(strata) > 1) {
        for (j in 2:length(strata)) {
          if (length(strata[[j]]) != n) 
            stop("all elements in strata must be same length")
        }
      }
      snms <- c()
      maxStrat <- max(unique(strata[[1]]))
      if (stratanm) {
        for (k in 1:length(strata)) {
          snms[k] <- as.character(cl[[4]])[k + 1]
        }
      }
      if (is.null(snms)) 
        snms <- rep("", length(strata))
      tmp <- paste(snms[1], format(strata[[1]]))
      if (length(strata) > 1) {
        for (i in 2:length(strata)) tmp <- paste(tmp, 
                                                 snms[i], format(strata[[i]]))
      }
    }
    else {
      if (is.null(dim(strata))) {
        snms <- deparse(cl$strata)
      }
      else {
        snms <- dimnames(strata)[[2]]
      }
      strata <- as.matrix(strata, drop = FALSE)
      maxStrat <- max(unique(strata[, 1]))
      if (is.null(snms)) {
        snms <- c()
        if (stratanm) {
          for (k in 1:dim(strata)[2]) {
            snms[k] <- as.character(cl[[4]])[k + 1]
          }
          if (dim(strata)[2] == 1) {
            snms <- deparse(cl[[4]])
          }
        }
        else {
          snms <- rep("", length(strata))
        }
      }
      tmp <- paste(snms[1], format(strata[, 1]))
      if (dim(strata)[2] > 1) {
        for (i in 2:(dim(strata)[2])) tmp <- paste(tmp, 
                                                   snms[i], format(strata[, i]))
      }
    }
    strata <- tmp
  }
  if (snms[1] == "" & length(cl) > 3) {
    if (grepl("strata", deparse(cl))) {
      snms <- deparse(cl[[4]])
      if (grepl("cbind", snms)) {
        snms <- strsplit("cbind", snms)[[1]][2]
      }
    }
    snms <- deparse(cl[[3]])
  }
  meancex = cex[1]
  jittercex = cex[2]
  if (log == TRUE) {
    log = "y"
  }
  if (log == FALSE) {
    log = ""
  }
  sdcol <- col[1]
  jittercol <- col[2]
  l <- length(unique(strata))
  byvar <- sort(unique(strata))
  bigy <- y
  bigx <- x
  bigdat <- data
  outList <- list()
  xList <- list()
  yList <- list()
  ylim <- c(min(y), max(y))
  if (!legend) {
    if (min(x) > 0) {
      if (length(unique(x)) == 1) {
        xlim <- c(0.5, l * (length(unique(x))) + 0.5)
      }
      else {
        xlim <- c(0.5, l * (length(unique(x))) + 0.5)
      }
    }
    else {
      if (max(x) > 1) {
        if (length(unique(x)) == 1) {
          xlim <- c(0.5, l * (length(unique(x))) + 0.5)
        }
        else {
          xlim <- c(0.5, l * (length(unique(x))) + 0.5)
        }
        if (l > 2) {
          if (length(unique(x)) == 1) {
            xlim <- c(0.5, l * (length(unique(x))))
          }
          else {
            xlim <- c(0.5, l * (length(unique(x))))
          }
        }
      }
      else {
        if (length(unique(x)) == 1) {
          xlim <- c(0.5, l * (length(unique(x))) + 0.5)
        }
        else {
          xlim <- c(0.5, l * (length(unique(x))) + 0.5)
        }
      }
    }
  }
  else {
    if (min(x) > 0) {
      xlim <- c(-0.25, l * (length(unique(x))) + 0.5)
    }
    else {
      if (max(x) > 1) {
        xlim <- c(-0.25, l * (length(unique(x))) + 0.5)
        if (l > 2) {
          xlim <- c(-0.25, l * (length(unique(x))))
        }
      }
      else {
        xlim <- c(-0.25, l * (length(unique(x))) + 0.5)
      }
    }
  }
  addOn <- 0
  leftRange <- 0
  rightRange <- 0
  for (j in 1:l) {
    newy <- subset(bigy, strata == byvar[j])
    newx <- subset(bigx, strata == byvar[j])
    if (length(data) == 1) {
      if (is.na(data)) {
        out <- boxplot(newy ~ newx, at = 1:max(newx), 
                       range = range, log = log, outpch = NA, plot = F,names=names)
        if (j > 1) {
          if (leftRange == 0) {
            leftRange <- max(newx) + 2
          }
          addOn <- (j - 1) * max(newx)
          rightRange <- max(newx) + addOn
          out <- boxplot(newy ~ newx, at = leftRange:rightRange, 
                         range = range, log = log, outpch = NA, plot = F,names=names)
        }
      }
    }
    if (length(data) != 1) {
      out <- boxplot(newy ~ newx, at = 1:max(newx), range = range, 
                     data = data, log = log, outpch = NA, plot = F,names=names)
      if (j > 1) {
        if (leftRange == 0) {
          leftRange <- max(newx) + 1
        }
        addOn <- (j - 1) * max(newx)
        rightRange <- max(newx) + addOn
        out <- boxplot(newy ~ newx, at = leftRange:rightRange, 
                       range = range, data = data, log = log, outpch = NA, 
                       plot = F,names=names)
      }
    }
    outList[[j]] <- out
    yList[[j]] <- newy
    xList[[j]] <- newx
    leftRange <- rightRange + 2
  }
  if (stratanm) {
    subj <- c(deparse(substitute(strata)))
    namej <- c(deparse(substitute(strata)))
    if (length(cl) >= 3) {
      if (deparse(cl[[3]]) != "TRUE") {
        namej <- c(deparse(cl[[3]]))
      }
      else {
        namej <- c(deparse(cl[[2]]))
      }
    }
  }
  else {
    subj <- ""
    if (length(cl) >= 3) {
      if (deparse(cl[[3]]) != "TRUE") {
        namej <- c(deparse(cl[[3]]))
      }
      else {
        namej <- c(deparse(cl[[2]]))
      }
    }
    else {
      namej <- c(deparse(cl[[2]]))
    }
  }
  mymain <- main
  if (snms[1] != "" & is.null(mymain)) {
    mymain <- paste("Stratified Boxplot by:", snms[1])
    if (length(snms) > 1) {
      for (i in 1:(length(snms) - 1)) {
        mymain <- paste(mymain, ", ", snms[i + 1], sep = "")
      }
    }
  }
  if (stratanm) {
    xlab2 <- byvar
    if (length(snms) > 1) {
      for (i in 1:length(xlab2)) {
        vec <- strsplit(xlab2[i], " ")[[1]]
        str <- NULL
        for (j in 1:length(vec)) {
          if (j == 2) {
            str <- paste(str, vec[j], sep = "")
          }
          else if (j > 2 & j%%2 == 0) {
            str <- paste(str, vec[j], sep = ",")
          }
          else {
          }
        }
        xlab2[i] <- str
      }
    }
    sub <- snms[1]
    if (length(snms) > 1) {
      for (i in 2:length(snms)) {
        sub <- paste(sub, ", ", snms[i], sep = "")
      }
    }
  }
  else if (l > 1) {
    xlab2 <- byvar
    sub <- ""
  }
  else {
    xlab2 <- xlab
    sub <- ""
  }
  if (length(namej) > 1) {
    xlab2 <- deparse(cl[[3]])
  }
  if (is.null(ylab)) {
    ylab <- deparse(cl[[2]])
  }
  if (length(snms) > 1) {
    bxp(outList[[1]], log = log, main = mymain, ylab = ylab, 
        names = names, ylim = ylim, xlim = xlim)
    if (min(xList[[1]]) > 0) {
      mtext(names[2], at = (xlim[1] + xlim[2])/2, line = 2, 
            side = 1)
      mtext(xlab2[1], at = (min(xList[[1]]) + max(xList[[1]]))/2, 
            line = 3, side = 1)
      mtext(sub, at = (xlim[1] + xlim[2])/2, line = 4, 
            side = 1)
    }
    else {
      if (max(xList[[1]]) == 1) {
        mtext(names[2], at = (xlim[1] + xlim[2])/2, line = 2, 
              side = 1)
        mtext(xlab2[1], at = 1.5, line = 3, side = 1)
        mtext(sub, at = (xlim[1] + xlim[2])/2, line = 4, 
              side = 1)
      }
      else {
        if (!is.null(xlab)) {
          mtext(xlab, at = (xlim[1] + xlim[2])/2, line = 2, 
                side = 1)
        }
        else {
          mtext(names[2], at = (xlim[1] + xlim[2])/2, 
                line = 2, side = 1)
        }
        mtext(xlab2[1], at = max(xList[[1]])/2 + 1, line = 3, 
              side = 1)
        mtext(sub, at = (xlim[1] + xlim[2])/2, line = 4, 
              side = 1)
      }
    }
  }
  else {
    bxp(outList[[1]], log = log, main = mymain, ylab = ylab, 
        sub = sub, names = names, ylim = ylim, xlim = xlim,names=names)
    if (!is.null(xlab)) {
      mtext(xlab, at = (xlim[1] + xlim[2])/2, line = 2, 
            side = 1)
    }
    else {
      mtext(names[2], at = (xlim[1] + xlim[2])/2, line = 2, 
            side = 1)
    }
    if (min(xList[[1]]) > 0) {
      mtext(xlab2[1], at = (min(xList[[1]]) + max(xList[[1]]))/2, 
            line = 3, side = 1)
    }
    else {
      if (max(xList[[1]]) == 1) {
        mtext(xlab2[1], at = 1.5, line = 3, side = 1)
      }
      else {
        if (length(xlab2) != length(xlab)) {
          mtext(xlab2[1], at = max(xList[[1]])/2 + 1, 
                line = 3, side = 1)
        }
      }
    }
  }
  if (xjitter == TRUE) {
    if (yjitter == TRUE) {
      stripchart(jitter(yList[[1]]) ~ xList[[1]], vertical = TRUE, 
                 method = "jitter", pch = 21 + j - 1, cex = jittercex, 
                 col = jittercol, bg = jittercol, add = TRUE)
    }
    else {
      stripchart(yList[[1]] ~ xList[[1]], vertical = TRUE, 
                 method = "jitter", pch = 21 + j - 1, cex = jittercex, 
                 col = jittercol, bg = jittercol, add = TRUE)
    }
  }
  if (sd == TRUE) {
    nonZero <- TRUE
    for (k in 1:length(unique(xList[[1]]))) {
      subdat <- subset(yList[[1]], xList[[1]] == unique(x)[order(unique(x))][k])
      submean <- mean(subdat)
      subsd <- sd(subdat)
      if (unique(x)[order(unique(x))][1] == 0) {
        nonZero <- FALSE
      }
      if (nonZero) {
        i <- as.numeric(unique(x)[order(unique(x))][k])
      }
      else {
        i <- as.numeric(unique(x)[order(unique(x))][k]) + 
          1
      }
      if (sdx == TRUE) {
        segments(i - 0.15, submean + subsd, i + 0.15, 
                 submean - subsd, col = sdcol, lwd = 0.75)
        segments(i + 0.15, submean + subsd, i - 0.15, 
                 submean - subsd, col = sdcol, lwd = 0.75)
      }
      segments(i - 0.15, submean + subsd, i - 0.15, submean - 
                 subsd, col = sdcol, lwd = 0.75)
      segments(i + 0.15, submean + subsd, i + 0.15, submean - 
                 subsd, col = sdcol, lwd = 0.75)
      segments(i - 0.15, submean + subsd, i + 0.15, submean + 
                 subsd, col = sdcol, lwd = 0.75)
      segments(i - 0.15, submean - subsd, i + 0.15, submean - 
                 subsd, col = sdcol, lwd = 0.75)
      segments(i - 0.25, submean, i + 0.25, submean, col = sdcol, 
               lwd = 1.1)
      points(i, submean, col = sdcol, pch = 21 + j - 1, 
             cex = meancex, bg = sdcol)
      if (legend == TRUE & k == 1) {
        if (unique(x)[order(unique(x))][k] == 0) {
          left <- 0.6
        }
        else {
          left <- unique(x)[order(unique(x))][k] - 1.5
        }
        text(xlim[1] + 0.5, max(subdat), "Max", cex = 0.65)
        text(xlim[1] + 0.5, mean(subdat) + sd(subdat), 
             "Mn+SD", cex = 0.65, col = "blue")
        text(xlim[1] + 0.5, mean(subdat), "Mn", cex = 0.65, 
             col = "blue")
        text(xlim[1] + 0.5, mean(subdat) - sd(subdat), 
             "Mn-SD", cex = 0.65, col = "blue")
        text(xlim[1] + 0.5, min(subdat), "Min", cex = 0.65)
        if (range > 0) {
          if (outList[[1]]$stats[5] == max(subdat)) {
            text(xlim[1] + 0.5, outList[[1]]$stats[5] - 
                   1, paste("P75+", range, "IQR", sep = ""), 
                 cex = 0.65)
          }
          else {
            text(xlim[1] + 0.5, outList[[1]]$stats[5], 
                 paste("P75+", range, "IQR", sep = ""), 
                 cex = 0.65)
          }
          if (outList[[1]]$stats[1] == min(subdat)) {
            text(xlim[1] + 0.5, outList[[1]]$stats[1] + 
                   1, paste("P25+", range, "IQR", sep = ""), 
                 cex = 0.65)
          }
          else {
            text(xlim[1] + 0.5, outList[[1]]$stats[1], 
                 paste("P25+", range, "IQR", sep = ""), 
                 cex = 0.65)
          }
        }
      }
    }
  }
  leftRange <- 0
  rightRange <- 0
  if (l > 1) {
    for (m in 2:length(outList)) {
      if (leftRange == 0) {
        if (min(xList[[m]]) == 0 & length(unique(xList[[m]])) != 
            1) {
          leftRange <- max(xList[[m]]) + 3
        }
        else if (length(unique(xList[[m]])) == 1) {
          leftRange <- 2
        }
        else {
          leftRange <- max(xList[[m]]) + 2
        }
      }
      addOn <- min(length(unique(x)), length(unique(xList[[m]]))) - 
        1
      if (max(xList[[m]]) == 1 & length(unique(xList[[m]])) != 
          1) {
        rightRange <- leftRange + 1
      }
      else {
        rightRange <- leftRange + addOn
      }
      bxp(outList[[m]], at = leftRange:rightRange, log = log, 
          names = names, ylim = ylim, xlim = xlim, add = TRUE)
      mtext(xlab2[m], side = 1, line = 3, at = (leftRange + 
                                                  rightRange)/2)
      if (xjitter == TRUE) {
        if (yjitter == TRUE) {
          stripchart(jitter(yList[[m]]) ~ xList[[m]], 
                     vertical = TRUE, method = "jitter", pch = 21 + 
                       j - 1, cex = jittercex, col = jittercol, 
                     bg = jittercol, add = TRUE, at = leftRange:rightRange)
        }
        else {
          stripchart(yList[[m]] ~ xList[[m]], vertical = TRUE, 
                     method = "jitter", pch = 21 + j - 1, cex = jittercex, 
                     col = jittercol, bg = jittercol, add = TRUE, 
                     at = leftRange:rightRange)
        }
      }
      if (sd == TRUE) {
        nonZero <- TRUE
        for (k in 1:length(unique(xList[[m]]))) {
          subdat <- subset(yList[[m]], xList[[m]] == 
                             unique(x)[order(unique(x))][k])
          submean <- mean(subdat)
          subsd <- sd(subdat)
          if (unique(x)[order(unique(x))][1] == 0) {
            nonZero <- FALSE
          }
          if (nonZero) {
            i <- as.numeric(unique(x + (m - 1) * max(x))[order(unique(x + 
                                                                        (m - 1) * max(x)))][k])
          }
          else {
            if (k == 1) {
              i <- leftRange
            }
            else {
              i <- leftRange + (k - 1)
            }
          }
          if (sdx == TRUE) {
            segments(i - 0.15, submean + subsd, i + 0.15, 
                     submean - subsd, col = sdcol, lwd = 0.75)
            segments(i + 0.15, submean + subsd, i - 0.15, 
                     submean - subsd, col = sdcol, lwd = 0.75)
          }
          segments(i - 0.15, submean + subsd, i - 0.15, 
                   submean - subsd, col = sdcol, lwd = 0.75)
          segments(i + 0.15, submean + subsd, i + 0.15, 
                   submean - subsd, col = sdcol, lwd = 0.75)
          segments(i - 0.15, submean + subsd, i + 0.15, 
                   submean + subsd, col = sdcol, lwd = 0.75)
          segments(i - 0.15, submean - subsd, i + 0.15, 
                   submean - subsd, col = sdcol, lwd = 0.75)
          segments(i - 0.25, submean, i + 0.25, submean, 
                   col = sdcol, lwd = 1.1)
          points(i, submean, col = sdcol, pch = 21 + 
                   j - 1, cex = meancex, bg = sdcol)
        }
      }
      if (length(unique(xList[[m]])) != 1) {
        leftRange <- rightRange + 2
      }
      else if (length(snms) > 1) {
        leftRange <- rightRange + 2.5
      }
      else {
        leftRange <- rightRange + 1
      }
    }
  }
}