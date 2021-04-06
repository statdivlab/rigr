## A function to perform lm, glm, correlated data regression, and report multiple partial f-tests
## Args: fnctl         - the functional to calculate, e.g. "mean"
##       formula       - the formula to compute
##       data          - the data frame or matrix to use. if missing, assumed to be attached
##       intercept     - whether or not to include an intercept. no intercept for hazard regression
##       strata        - stratified regression
##       weights       - the weights for the regression
##       id            - id for correlated data regression
##       ties          - method for breaking ties in hazard regression
##       subset        - subset of the data for the regression
##       robustSE      - compute robustSE standard error estimates
##       conf.level    - confidence level for tests
##       exponentiate  - return a table with exponentiated estimates and confidence intervals
##       replaceZeroes - the value to replace zeroes in the data with
##       useFdstn      - use the f-distribution for tests
##       suppress      - suppress the printing of the raw model table
##       na.action     - how to deal with NAs in the data
##       method        - the method for regression
##       model.f       - return the model frame
##       model.x       - return the x values entered
##       model.y       - return the y vector
##       qr            - qr method
##       singular.ok   - takes singular design matrices
##       contrasts     - special contrasts for regression
##       offset        - any offset
##       control       - 
##       init          - for cox regression
##       ...           - other arbitrary arguments to lower level functions
##       version       - the version of the function
## Returns: a uRegress object, with a list of tables and tests and coefficients
## Version: 2015 06 02
regress <-
  function(fnctl, formula, data,                     # the terms in the regression analysis
           intercept = fnctl != "hazard", 
           strata = rep(1,n), weights = rep(1,n), id = 1:n, ties = "efron", subset = rep(TRUE,n),
           robustSE = TRUE, conf.level = 0.95, exponentiate=fnctl != "mean",
           replaceZeroes, useFdstn = TRUE, suppress = FALSE, na.action, method = "qr", model.f = TRUE, model.x = FALSE, model.y = FALSE, qr = TRUE,
           singular.ok = TRUE, contrasts = NULL, offset,control = list(...), init, ..., version = FALSE) {
    
    vrsn <- "20160301"
    if (version) return(vrsn)
    
    cl <- match.call()
    fit <- NULL
    if(missing(formula)){
      stop("You must enter a formula")
    }
    # checking functional  fnctl
    findx <-  pmatch(fnctl,c("mean", "geometric mean", "odds", "rate", "hazard"))
    if (is.na(findx)) stop("unsupported functional")
    fnctl <- c("mean", "geometric mean", "odds", "rate", "hazard")[findx]
    if(fnctl=="hazard") stop("proportional hazards regression no longer supported")
    if (intercept & fnctl=="hazard") stop("hazard regression cannot include an intercept")
    glm <- FALSE
    ## get the family for glm if we need it
    if(fnctl%in%c("odds", "rate")){
      glm <- TRUE
      if(method=="qr") method <- "glm.fit"
      if(fnctl=="odds"){
        family <- "binomial"
      } else {
        family <- "poisson"
      }
    } else {
      if(fnctl!="hazard"){
        family="gaussian"
      }
    }
    if(!intercept){
      form <- deparse(formula)
      if(length(form) > 1) {
        form <- paste(form, collapse = "")
      }
      form <- paste(form, "-1")
      formula <- as.formula(form, env=.GlobalEnv)
    }
    ## Set up the model matrix and formula
    if(fnctl != "hazard"){
      cl <- match.call()
      mf <- match.call(expand.dots = FALSE)
      m <- match(c("formula", "data", "subset", "weights", "na.action", 
                   "offset"), names(mf), 0L)
      if(glm){
        m <- match(c("formula", "data", "subset", "weights", "na.action", 
                     "etastart", "mustart", "offset"), names(mf), 0L)
      }
    } else {
      replaceZeroes <- NA
      msng <- FALSE
      ties <- match.arg(ties)
      Call <- match.call()
      indx <- match(c("formula", "data", "weights", "subset", "na.action"), 
                    names(Call), nomatch = 0)
      if (indx[1] == 0) 
        stop("A formula argument is required")
      mf <- Call[c(1, indx)]
      mf[[1]] <- as.name("model.frame")
      #special <- c("strata", "cluster", "tt")
      special <- c("strata", "cluster")
    }
    ## Get the correct formula and any multiple-partial F-tests
    testlst <- testList(formula, mf, m, data)
    formula <- testlst$formula
    ## length is zero if no tests
    tmplist <- testlst$testList
    ## get the terms lists
    termlst <- testlst$termList
    mf$formula <- formula
    mftmp <- mf
    mfGee <- mf
    mG <- match(c("formula", "data", "subset", "weights", "na.action", 
                  "offset", "id"), names(mfGee), 0L)
    mfGee <- mfGee[c(1L, mG)]
    mfGee[[1]] <- as.name("model.frame")
    mfGee <- eval(mfGee, parent.frame())
    id <- model.extract(mfGee, id)    
    
    ## Evaluate the formula and get the correct returns
    if(fnctl != "hazard"){  
      ret.x <- model.x
      ret.y <- model.y
      if(any(m)>0){
        mf <- mf[c(1L, m)]
        mf$drop.unused.levels <- TRUE
        mf[[1L]] <- quote(stats::model.frame)
        mf <- eval(mf, parent.frame())
      }
      if(method=="model.frame") return(mf)
      else if (!glm){
        if (method != "qr") 
          warning(gettextf("method = '%s' is not supported. Using 'qr'", 
                           method), domain = NA)
        mt <- attr(mf, "terms")
        factorMat <- NULL
        term.labels <- NULL
        if(length(tmplist)>0){
          factorMat <- attr(mt, "factors")
          term.labels <- attr(mt, "term.labels")
          for(i in 1:length(tmplist)){
            tmpFactors <- attr(attr(tmplist[[i]], "terms"), "factors")
            tmpVec <- as.matrix(apply(tmpFactors, 1, sum))
            tmpCol <- matrix(rep(0, dim(factorMat)[1]))
            tmpCol[match(dimnames(tmpVec)[[1]], dimnames(factorMat)[[1]]),] <- tmpVec
            dimnames(tmpCol) <- list(dimnames(factorMat)[[1]], names(tmplist)[[i]])
            factorMat <- cbind(factorMat, tmpCol)
            ter <- attr(termlst[[i+1]], "term.labels")
            hasU <- grepl("U", ter)
            if(any(hasU)){
              tmpter <- ter[hasU] 
              tmpEqter <- tmpter[grepl("=", tmpter, fixed=TRUE)]
              tmpEqter <- unlist(strsplit(tmpEqter, "U", fixed=TRUE))
              if(sum(grepl(")", tmpEqter, fixed=TRUE))!=length(tmpEqter)-1){
                tmpEqter <- tmpEqter[-which(grepl(")", tmpEqter, fixed=TRUE))]
              }
              tmpEqter <- as.matrix(tmpEqter)
              tmpEqter <- apply(tmpEqter, 1, splitOnParen)
              tmpEqter <- unlist(lapply(strsplit(tmpEqter, "=", fixed=TRUE), function(x) return(x[1])))
              tmpEqter <- unlist(lapply(strsplit(tmpEqter, " ", fixed=TRUE), function(x) return(x[1])))
              tmpEqter[is.na(tmpEqter)] <- ""
              tmpter[grepl("=", tmpter, fixed=TRUE)] <- pasteTwo(tmpEqter)
              ter[hasU] <- tmpter
            }
            term.labels <- c(term.labels, ter)
            term.labels <- unique(term.labels)
            dimnames(tmplist[[i]])[[2]] <- paste(names(tmplist)[[i]], dimnames(tmplist[[i]])[[2]],sep=".")
          }
        }
        y <- model.response(mf, "numeric")
        # checking response variable y  
        if (fnctl=="geometric mean") {
          if (missing(replaceZeroes)) replaceZeroes <- min(y[y>0]/2)
          y <- log(ifelse(y<=0,replaceZeroes,y))
        } else replaceZeroes <- NA
        n <- length(y)
        msng <- FALSE
        if(!missing(strata)){msng <- TRUE}
        if (!is.null(strata) & msng & fnctl != "hazard") warning("Strata ignored unless hazard regression")
        if (!is.null(strata) & length(strata) != n) stop("Response variable and strata must be of same length")
        if (length(weights) != n) stop("Response variable and weights must be of same length")
        if (length(subset) != n) stop("Response variable and subsetting variable must be of same length")
        
        w <- as.vector(model.weights(mf))
        if (!is.null(w) && !is.numeric(w)) 
          stop("'weights' must be a numeric vector")
        offset <- as.vector(model.offset(mf))
        if (!is.null(offset)) {
          if (length(offset) != NROW(y)) 
            stop(gettextf("number of offsets is %d, should equal %d (number of observations)", 
                          length(offset), NROW(y)), domain = NA)
        }
        if (is.empty.model(mt)) {
          x <- NULL
          z <- list(coefficients = if (is.matrix(y)) matrix(, 0, 
                                                            3) else numeric(), residuals = y, fitted.values = 0 * 
                      y, weights = w, rank = 0L, df.residual = if (!is.null(w)) sum(w != 
                                                                                      0) else if (is.matrix(y)) nrow(y) else length(y))
          if (!is.null(offset)) {
            z$fitted.values <- offset
            z$residuals <- y - offset
          }
        } else {
          x <- model.matrix(mt, mf, contrasts)
          
          z <- if (is.null(w)) 
            lm.fit(x, y, offset = offset, singular.ok = singular.ok, 
                   ...)
          else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok, 
                       ...)
        }
        class(z) <- c(if (is.matrix(y)) "mlm", "lm")
        z$na.action <- attr(mf, "na.action")
        z$offset <- offset
        z$contrasts <- attr(x, "contrasts")
        z$xlevels <- .getXlevels(mt, mf)
        z$call <- cl
        z$terms <- mt
        if (model.f) 
          z$model <- mf
        if (ret.x) 
          z$x <- x
        if (ret.y) 
          z$y <- y
        if (!qr) 
          z$qr <- NULL
        model <- x
        fit <- z
      } else {
        replaceZeroes <- NA
        if (is.character(family)) 
          family <- get(family, mode = "function", envir = parent.frame())
        if (is.function(family)) 
          family <- family()
        if (is.null(family$family)) {
          print(family)
          stop("'family' not recognized")
        }
        if (!is.character(method) && !is.function(method)) 
          stop("invalid 'method' argument")
        if(missing(control)) control <- glm.control(...)
        if (identical(method, "glm.fit")) 
          control <- do.call("glm.control", control)
        mt <- attr(mf, "terms")
        factorMat <- NULL
        term.labels <- NULL
        if(length(tmplist)>0){
          factorMat <- attr(mt, "factors")
          term.labels <- attr(mt, "term.labels")
          for(i in 1:length(tmplist)){
            tmpFactors <- attr(attr(tmplist[[i]], "terms"), "factors")
            tmpVec <- as.matrix(apply(tmpFactors, 1, sum))
            tmpCol <- matrix(rep(0, dim(factorMat)[1]))
            tmpCol[match(dimnames(tmpVec)[[1]], dimnames(factorMat)[[1]]),] <- tmpVec
            dimnames(tmpCol) <- list(dimnames(factorMat)[[1]], names(tmplist)[[i]])
            factorMat <- cbind(factorMat, tmpCol)
            ter <- attr(termlst[[i+1]], "term.labels")
            hasU <- grepl("U", ter)
            if(any(hasU)){
              tmpter <- ter[hasU] 
              tmpEqter <- tmpter[grepl("=", tmpter, fixed=TRUE)]
              tmpEqter <- unlist(strsplit(tmpEqter, "U", fixed=TRUE))
              tmpEqter <- as.matrix(tmpEqter)
              tmpEqter <- apply(tmpEqter, 1, splitOnParen)
              tmpEqter <- unlist(lapply(strsplit(tmpEqter, "=", fixed=TRUE), function(x) return(x[1])))
              tmpEqter[is.na(tmpEqter)] <- ""
              tmpter[grepl("=", tmpter, fixed=TRUE)] <- pasteTwo(tmpEqter)
              ter[hasU] <- tmpter
            }
            term.labels <- c(term.labels, ter)
            term.labels <- unique(term.labels)
            dimnames(tmplist[[i]])[[2]] <- paste(names(tmplist)[[i]], dimnames(tmplist[[i]])[[2]],sep=".")
          }
        }
        y <- model.response(mf, "any")
        if (length(dim(y)) == 1L) {
          nm <- rownames(y)
          dim(y) <- NULL
          if (!is.null(nm)) 
            names(y) <- nm
        }
        x <- if (!is.empty.model(mt)) 
          model.matrix(mt, mf, contrasts)
        else matrix(, NROW(y), 0L)
        n <- NROW(y)
        msng <- FALSE
        if(!missing(strata)){msng <- TRUE}
        if (!is.null(strata) & msng & fnctl != "hazard") warning("Strata ignored unless hazard regression")
        if (!is.null(strata) & length(strata) != NROW(y)) stop("Response variable and strata must be of same length")
        if (length(weights) != NROW(y)) stop("Response variable and weights must be of same length")
        if (length(subset) != NROW(y)) stop("Response variable and subsetting variable must be of same length")
        w <- as.vector(model.weights(mf))
        if (!is.null(w) && !is.numeric(w)) 
          stop("'weights' must be a numeric vector")
        if (!is.null(w) && any(w < 0)) 
          stop("negative weights not allowed")
        offset <- as.vector(model.offset(mf))
        if (!is.null(offset)) {
          if (length(offset) != NROW(y)) 
            stop(gettextf("number of offsets is %d should equal %d (number of observations)", 
                          length(offset), NROW(y)), domain = NA)
        }
        start <- model.extract(mf, "start")
        mustart <- model.extract(mf, "mustart")
        etastart <- model.extract(mf, "etastart")
        
        fit <- eval(call(if (is.function(method)) "method" else method, 
                         x = x, y = y, weights = w, start = start, etastart = etastart, 
                         mustart = mustart, offset = offset, family = family, 
                         control = control, intercept = attr(mt, "intercept") > 
                           0L))
        if (length(offset) && attr(mt, "intercept") > 0L) {
          fit2 <- eval(call(if (is.function(method)) "method" else method, 
                            x = x[, "(Intercept)", drop = FALSE], y = y, weights = w, 
                            offset = offset, family = family, control = control, 
                            intercept = TRUE))
          if (!fit2$converged) 
            warning("fitting to calculate the null deviance did not converge -- increase 'maxit'?")
          fit$null.deviance <- fit2$deviance
        }
        if (model.f) 
          fit$model <- mf
        fit$na.action <- attr(mf, "na.action")
        if (model.x) 
          fit$x <- x
        if (!model.y) 
          fit$y <- NULL
        fit <- c(fit, list(call = call, formula = formula, terms = mt, 
                           data = data, offset = offset, control = control, method = method, 
                           contrasts = attr(x, "contrasts"), xlevels = .getXlevels(mt, 
                                                                                   mf)))
        class(fit) <- c(fit$class, c("glm", "lm"))
      }
      model <- x
      
    } else { # if it is hazard, it is coxph
      ties <- match.arg(ties)
      Call <- match.call()
      if(any(grepl("id", names(Call)))){
        names(Call)[which(names(Call)=="id")] <- "cluster"
        robust <- TRUE
      } else {
        robust <- FALSE
      }
      indx <- match(c("formula", "data", "weights", "subset", "na.action"), 
                    names(Call), nomatch = 0)
      if (indx[1] == 0) 
        stop("A formula argument is required")
      temp <- Call[c(1, indx)]
      temp[[1]] <- as.name("model.frame")
      coxform <- formula
      if(any(grepl("cluster", names(Call)))){
        tmpform <- deparse(formula)
        tmpform <- paste(tmpform, "+ cluster(", Call$cluster, ")", sep="")
        coxform <- as.formula(tmpform, .GlobalEnv)
      }
      
      temp$formula <- coxform
      
      special <- c("strata", "cluster")
      if (missing(data)){
        temp$formula <- terms(coxform, special)
      } else {
        temp$formula <- terms(coxform, special, data = data)
      }
      method <- ties
      formula <- temp$formula
      mf <- eval(temp, parent.frame())
      if (nrow(mf) == 0) 
        stop("No (non-missing) observations")
      Terms <- terms(mf)
      mt <- Terms
      factorMat <- NULL
      term.labels <- NULL
      if(length(tmplist)>0){
        factorMat <- attr(mt, "factors")
        term.labels <- attr(mt, "term.labels")
        for(i in 1:length(tmplist)){
          tmpFactors <- attr(attr(tmplist[[i]], "terms"), "factors")
          tmpVec <- as.matrix(apply(tmpFactors, 1, sum))
          tmpCol <- matrix(rep(0, dim(factorMat)[1]))
          tmpCol[match(dimnames(tmpVec)[[1]], dimnames(factorMat)[[1]]),] <- tmpVec
          dimnames(tmpCol) <- list(dimnames(factorMat)[[1]], names(tmplist)[[i]])
          factorMat <- cbind(factorMat, tmpCol)
          ter <- attr(termlst[[i+1]], "term.labels")
          hasU <- grepl("U", ter)
          if(any(hasU)){
            tmpter <- ter[hasU] 
            tmpEqter <- tmpter[grepl("=", tmpter, fixed=TRUE)]
            tmpEqter <- unlist(strsplit(tmpEqter, "U", fixed=TRUE))
            tmpEqter <- as.matrix(tmpEqter)
            tmpEqter <- apply(tmpEqter, 1, splitOnParen)
            tmpEqter <- unlist(lapply(strsplit(tmpEqter, "=", fixed=TRUE), function(x) return(x[1])))
            tmpEqter[is.na(tmpEqter)] <- ""
            tmpter[grepl("=", tmpter, fixed=TRUE)] <- pasteTwo(tmpEqter)
            ter[hasU] <- tmpter
          }
          term.labels <- c(term.labels, ter)
          term.labels <- unique(term.labels)
          dimnames(tmplist[[i]])[[2]] <- paste(names(tmplist)[[i]], dimnames(tmplist[[i]])[[2]],sep=".")
        }
      }
      extraArgs <- list(...)
      if (length(extraArgs)) {
        controlargs <- names(formals(coxph.control))
        indx <- pmatch(names(extraArgs), controlargs, nomatch = 0L)
        if (any(indx == 0L)) 
          stop(gettextf("Argument %s not matched", names(extraArgs)[indx == 
                                                                      0L]), domain = NA)
      }
      if (missing(control)) 
        control <- coxph.control(...)
      Y <- model.extract(mf, "response")
      if (!inherits(Y, "Surv")) 
        stop("Response must be a survival object")
      n <- dim(Y)[1]
      if(!missing(strata)){msng <- TRUE}
      if (!is.null(strata) & msng & fnctl != "hazard") warning("Strata ignored unless hazard regression")
      if (!is.null(strata) & length(strata) != n) stop("Response variable and strata must be of same length")
      if (length(weights) != n) stop("Response variable and weights must be of same length")
      if (length(subset) != n) stop("Response variable and subsetting variable must be of same length")
      type <- attr(Y, "type")
      if (type != "right" && type != "counting") 
        stop(paste("Cox model doesn't support \"", type, "\" survival data", 
                   sep = ""))
      w <- model.weights(mf)
      data.n <- nrow(Y)
      cluster <- attr(Terms, "specials")$cluster
      if (length(cluster)) {
        robust <- TRUE
        tempc <- untangle.specials(Terms, "cluster", 1:10)
        ord <- attr(Terms, "order")[tempc$terms]
        if (any(ord > 1)) 
          stop("Cluster can not be used in an interaction")
        cluster <- strata(mf[, tempc$vars], shortlabel = TRUE)
        dropterms <- tempc$terms
        dropcon <- tempc$vars
        xlevels <- .getXlevels(Terms[-tempc$terms], mf)
      }
      else {
        dropterms <- dropcons <- NULL
        xlevels <- .getXlevels(Terms, mf)
      }
      strats <- attr(Terms, "specials")$strata
      if (length(strats)) {
        stemp <- untangle.specials(Terms, "strata", 1)
        if (length(stemp$vars) == 1) 
          strata.keep <- mf[[stemp$vars]]
        else strata.keep <- strata(mf[, stemp$vars], shortlabel = TRUE)
        strats <- as.numeric(strata.keep)
      }
      contrast.arg <- NULL
      attr(Terms, "intercept") <- TRUE
      adrop <- 0
      stemp <- untangle.specials(Terms, "strata", 1)
      if (length(stemp$vars) > 0) {
        hasinteractions <- FALSE
        for (i in stemp$vars) {
          if (any(attr(Terms, "order")[attr(Terms, "factors")[i, 
                                                              ] > 0] > 1)) 
            hasinteractions <- TRUE
        }
        if (!hasinteractions) 
          dropterms <- c(dropterms, stemp$terms)
        else adrop <- c(0, match(stemp$var, colnames(attr(Terms, 
                                                          "factors"))))
      }
      if (length(dropterms)) {
        temppred <- attr(terms, "predvars")
        Terms2 <- Terms[-dropterms]
        if (!is.null(temppred)) {
          attr(Terms2, "predvars") <- temppred[-(1 + dropterms)]
        }
        X <- model.matrix(Terms2, mf, constrasts = contrast.arg)
        renumber <- match(colnames(attr(Terms2, "factors")), 
                          colnames(attr(Terms, "factors")))
        attr(X, "assign") <- c(0, renumber)[1 + attr(X, "assign")]
      }
      else X <- model.matrix(Terms, mf, contrasts = contrast.arg)
      Xatt <- attributes(X)
      xdrop <- Xatt$assign %in% adrop
      X <- X[, !xdrop, drop = FALSE]
      attr(X, "assign") <- Xatt$assign[!xdrop]
      attr(X, "contrasts") <- Xatt$contrasts
      offset <- model.offset(mf)
      if (is.null(offset) | all(offset == 0)) 
        offset <- rep(0, nrow(mf))
      assign <- attrassign(X, Terms)
      contr.save <- attr(X, "contrasts")
      if (missing(init)) 
        init <- NULL
      if (method == "breslow" || method == "efron") {
        if (type == "right") 
          fitter <- get("coxph.fit")
        else fitter <- get("agreg.fit")
      }
      else if (method == "exact") {
        if (type == "right") 
          fitter <- get("coxexact.fit")
        else fitter <- get("agexact.fit")
      }
      else stop(paste("Unknown method", method))
      fit <- fitter(X, Y, strats, offset, init, control, weights = w, 
                    method = method, row.names(mf))
      
      if (is.character(fit)) {
        fit <- list(fail = fit)
        class(fit) <- "coxph"
      }
      else {
        if (!is.null(fit$coefficients) && any(is.na(fit$coefficients))) {
          vars <- (1:length(fit$coefficients))[is.na(fit$coefficients)]
          msg <- paste("X matrix deemed to be singular; variable", 
                       paste(vars, collapse = " "))
          if (singular.ok) 
            warning(msg)
          else stop(msg)
        }
        fit$n <- data.n
        fit$nevent <- sum(Y[, ncol(Y)])
        fit$terms <- Terms
        fit$assign <- assign
        class(fit) <- fit$method
        if (robust) {
          fit$naive.var <- fit$var
          fit$method <- method
          fit2 <- c(fit, list(x = X, y = Y, weights = w))
          if (length(strats)) 
            fit2$strata <- strats
          if (length(cluster)) {
            temp <- residuals(fit2, type = "dfbeta", 
                              collapse = cluster, weighted = TRUE)
            if (is.null(init)) 
              fit2$linear.predictors <- 0 * fit$linear.predictors
            else fit2$linear.predictors <- c(X %*% init)
            temp0 <- residuals(fit2, type = "score", 
                               collapse = cluster, weighted = TRUE)
          }
          else {
            temp <- residuals(fit2, type = "dfbeta", 
                              weighted = TRUE)
            fit2$linear.predictors <- 0 * fit$linear.predictors
            temp0 <- residuals(fit2, type = "score", 
                               weighted = TRUE)
          }
          fit$var <- t(temp) %*% temp
          u <- apply(as.matrix(temp0), 2, sum)
          fit$rscore <- coxph.wtest(t(temp0) %*% temp0, u, 
                                    control$toler.chol)$test
        }
        if (length(fit$coefficients) && is.null(fit$wald.test)) {
          nabeta <- !is.na(fit$coefficients)
          if (is.null(init)) 
            temp <- fit$coefficients[nabeta]
          else temp <- (fit$coefficients - init[1:length(fit$coefficients)])[nabeta]
          fit$wald.test <- coxph.wtest(fit$var[nabeta, nabeta], 
                                       temp, control$toler.chol)$test
        }
        na.action <- attr(mf, "na.action")
        if (length(na.action)) 
          fit$na.action <- na.action
        if (model.f) {
          fit$model <- mf
        }
        if (model.x) {
          fit$x <- X
          if (length(strats)) {
            fit$strata <- strata.keep
          }
        }
        if (model.y) 
          fit$y <- Y
      }
      if (!is.null(w) && any(w != 1)) 
        fit$weights <- w
      names(fit$means) <- names(fit$coefficients)
      fit$formula <- formula(Terms)
      if (length(xlevels) > 0) 
        fit$xlevels <- xlevels
      fit$contrasts <- contr.save
      if (any(offset != 0)) 
        fit$offset <- offset
      fit$call <- Call
      fit$method <- method
      model <- X
      y <- Y
    }
    
    ## Now we have the fit and the model
    ## First check to see if there are any repeated ids;
    ## then we use geeglm or coxph with clusters
    if(is.null(id)){
      id <- 1:n
    }
    anyRepeated <- any(table(id[!is.na(id)]) > 1)
    if(anyRepeated | !is.null(attr(formula, "specials")$cluster)){
      if(fnctl=="hazard"){
        fit <- if(!is.null(w)) coxph(formula, weights=w, data=data,...) else coxph(formula, data=data,...)
      } else if (fnctl %in% c("mean", "geometric mean")){
        if (fnctl=="geometric mean") {
          newy <- deparse(formula[[2]])
          newy <- paste("log(", newy, ")", sep="")
          form <- deparse(formula)
          form <- unlist(strsplit(form, "~", fixed=TRUE))[2]
          form <- paste(newy,"~", form, sep="")
          formula <- as.formula(form, env=.GlobalEnv)
        } 
        fit <- if(!is.null(w)) geepack::geeglm(formula, weights=w, family="gaussian",id=id, data=data,...) else geepack::geeglm(formula, family="gaussian",id=id, data=data,...)
      } else if (fnctl=="odds"){
        fit <- if(!is.null(w)) geepack::geeglm(formula, weights=w, family="binomial",id=id, data=data,...) else geepack::geeglm(formula, family="binomial",id=id, data=data,...)
      } else {
        fit <- if(!is.null(w)) geepack::geeglm(formula, weights=w, family="poisson",id=id, data=data,...) else geepack::geeglm(formula, family="poisson",id=id, data=data,...)
      }
    }
    
    ## Now I want to build the augmented coefficients matrix
    ## First I need to get the order of the predictors
    getn <- function(vec, n){
      if(n > length(vec)){
        return(vec)
      } else {
        return(vec[n:length(vec)])
      }
    }
    
    ## Formatting the terms and getting the correct ordering for the augmented Coefficients matrix
    z <- list(call=cl, terms=NULL,firstPred=NULL,lastPred=NULL,preds=NULL,X=NULL)
    terms <- attr(fit$terms, "term.labels")
    if(fnctl=="hazard" & !is.null(attr(fit$terms, "specials")$cluster)){
      terms <- terms[-length(terms)]
    }
    if(fnctl=="hazard" & !is.null(attr(fit$terms, "specials")$cluster)){
      model <- model.matrix(fit, data)
    } else{
      model <- model.matrix(fit)
    }
    preds <- dimnames(model)[[2]]
    preds1 <- preds
    hyperpreds <- dimnames(mf)[[2]]
    
    parens <- grepl(")", preds, fixed=TRUE)
    interact <- grepl(":", preds, fixed=TRUE)
    
    prList <- reFormatReg(preds, hyperpreds, mf)
    preds <- prList$preds
    args <- prList$args
    
    dimnames(model)[[2]] <- preds
    
    cols <- matrix(preds1, nrow=1)
    cols <- apply(cols, 2, createCols, terms)
    if(intercept) cols[1] <- "(Intercept)"
    
    names(fit$coefficients) <- preds
    
    p <- length(terms)
    for (i in 1:p) {
      z <- processTerm (z, model[,cols==terms[i]], terms[i])
    }
    tmp <- sapply(strsplit(z$preds, ".", fixed=TRUE), getn, n=2)
    if(is.list(tmp)){
      tmp <- lapply(tmp, paste, collapse=".")
    } else if (is.matrix(tmp)) {
      tmp <- apply(tmp, 2, paste, collapse=".")
    } else {
      
    }
    z$preds <- unlist(tmp)    
    z$preds[parens[-1]] <- paste(" ", z$preds[parens[-1]], sep="")
    
    z$X <- model
    model <- c(z, list(y=y,strata=strata,weights=weights,id=id,subset=subset))
    z <- c(model,list(fnctl=fnctl, intercept=intercept, exponentiate=exponentiate, replaceZeroes=replaceZeroes, 
                      conf.level=conf.level, useFdstn=useFdstn, original=model))
    
    ## Getting the names and setting up the augmented coefficients matrix
    nms <- c(z$terms[z$firstPred!=z$lastPred],z$preds)
    fst <- c(z$firstPred[z$firstPred!=z$lastPred],1:length(z$preds))
    lst <- c(z$lastPred[z$firstPred!=z$lastPred],1:length(z$preds))
    u <- order(fst,-lst)
    nms <- c(ifelse1(intercept,"Intercept",NULL),nms[u])
    fst <- c(ifelse1(intercept,0,NULL),fst[u]) + intercept
    lst <- c(ifelse1(intercept,0,NULL),lst[u]) + intercept
    cinames <- paste(format(100*conf.level),"%",c("L","H"),sep="")
    if (exponentiate) cinames <- paste("e(",c("Est",cinames),")",sep="")
    cinames <- c("Estimate",ifelse1(robustSE,c("Naive SE","Robust SE"),"Std Err"),cinames,
                 ifelse1(useFdstn,c("F stat","Pr(>F)"),c("Chi2 stat","Pr(>Chi2)")))
    secol <- 2 + robustSE
    if(anyRepeated){
      cinames2 <- paste(format(100*conf.level),"%",c("L","H"),sep="")
      if (exponentiate) cinames2 <- paste("e(",c("Est",cinames2),")",sep="")
      cinames2 <- c("Estimate", "Std Err",cinames2,"Wald", "Pr(>|W|)")
      cinames <- cinames2
      secol <- 1+robustSE
    }
    augCoefficients <- matrix(0,sum(z$firstPred!=z$lastPred)+length(z$pred)+intercept,length(cinames))
    dimnames(augCoefficients) <- list(nms,cinames)
    
    ## Getting the correct coefficients, robustSE standard errors, and f-statistics if needed
    if (fnctl == "hazard") {
      zzs <- summary(fit)
      converge <- NA
      n <- zzs$n
      zzs$coefficients <- zzs$coefficients[,-2,drop=F]
      if (robustSE) {
        zzs$robustCov <- fit$var
        zzs$naiveCov <- fit$naive.var
        scoreStat <- fit$rscore
      } else {
        zzs$naiveCov <- fit$var
        scoreStat <- fit$score
      }
      if(!anyRepeated & !robust){
        if (robustSE) {
          m <- sandwich(fit,adjust=T)
          zzs$coefficients <- cbind(zzs$coefficients[,1:2,drop=F],sqrt(diag(m)),zzs$coefficients[,-(1:2),drop=F])
          dimnames(zzs$coefficients)[[2]][2:3] <- c("Naive SE","Robust SE")
          zzs$robustCov <- m
        }
      }
      p <- dim(zzs$coefficients)[1]
      if (robustSE) m <- zzs$robustCov else m <- zzs$naiveCov
      zzs$coefficients[,secol+1] <- zzs$coefficients[,1] / zzs$coefficients[,secol]
      u <- (1+intercept):p
      waldStat <- t(zzs$coefficients[u,1]) %*% 
        (solve(m[u,u]) %*% zzs$coefficients[u,1]) / (p - intercept)
      LRStat <- 2 * diff(fit$loglik)
    } else {
      if(anyRepeated){
        zzs <- summary(fit)
        converge <- T
        zzs$naiveCov <- NA
        zzs$robustCov <- zzs$cov.unscaled
        n <- sum(zzs$df[1:2])
      } else if (fnctl %in% c("mean","geometric mean")) {
        zzs <- summary(fit)
        converge <- T
        zzs$naiveCov <-zzs$sigma^2 * zzs$cov.unscaled
        n <- sum(zzs$df[1:2])
      } else {
        converge <- fit$converged
        zzs <- summary(fit)
        zzs$naiveCov <- zzs$cov.scaled
        n <- zzs$df.null + intercept
      }
      if(!anyRepeated){
        if (robustSE) {
          m <- sandwich(fit,adjust=T)
          zzs$coefficients <- cbind(zzs$coefficients[,1:2,drop=F],sqrt(diag(m)),zzs$coefficients[,-(1:2),drop=F])
          dimnames(zzs$coefficients)[[2]][2:3] <- c("Naive SE","Robust SE")
          zzs$robustCov <- m
        }
      }
      p <- dim(zzs$coefficients)[1]
      if (robustSE) m <- zzs$robustCov else m <- zzs$naiveCov
      zzs$coefficients[,secol+1] <- zzs$coefficients[,1] / zzs$coefficients[,secol]
      u <- (1+intercept):p
      waldStat <- t(zzs$coefficients[u,1]) %*% 
        (solve(m[u,u]) %*% zzs$coefficients[u,1]) / (p - intercept)
      LRStat <- if (fnctl %in% c("mean","geometric mean")) 
        waldStat else (zzs$null.deviance - zzs$deviance) / (p - intercept) / zzs$deviance * (n-p)
      scoreStat <- NULL
      
    }
    
    p <- dim(zzs$coefficients)[1]
    
    if(!anyRepeated | fnctl == "hazard"){
      if (useFdstn) {
        waldStat <- c(waldStat,1-pf(waldStat,p-intercept,n-p),p-intercept,n-p)
        LRStat <- c(LRStat,1-pf(LRStat,p-intercept,n-p),p-intercept,n-p)
        if (!is.null(scoreStat)) scoreStat <- c(scoreStat,1-pf(scoreStat,p-intercept,n-p),p-intercept,n-p)
        zzs$coefficients[,secol+2] <- 2 * pt(- abs(zzs$coefficients[,secol+1]),df=n-p)
      } else {
        waldStat <- c(waldStat,1-pchisq(waldStat,p-intercept),p-intercept)
        LRStat <- c(LRStat,1-pchisq(LRStat,p-intercept),p-intercept)
        if (!is.null(scoreStat)) scoreStat <- c(scoreStat,1-pchisq(waldStat,p-intercept),p-intercept)
        zzs$coefficients[,secol+2] <- 2 * pnorm(- abs(zzs$coefficients[,secol+1]))
      }
    } 
    
    droppedPred <- is.na(fit$coefficients)
    if(anyRepeated){
      zzs$coefficients <- as.matrix(zzs$coefficients)
    }
    linearPredictor <- z$X[, !droppedPred] %*% zzs$coefficients[,1,drop=F]
    
    ## Creating the final uRegress object
    zzs$call <- cl
    zzs$droppedPred <- droppedPred
    tmp <- class(zzs)
    
    zzs <- c(zzs,list(augCoefficients=augCoefficients, waldStat=waldStat, LRStat=LRStat, scoreStat=scoreStat,
                      linearPredictor=linearPredictor, fit=fit, converge=converge))
    
    class(zzs) <- c("uRegress",tmp)
    if (useFdstn) ci <- qt((1-conf.level)/2,df=n-p) * zzs$coefficients[,secol] else ci <- qnorm((1-conf.level)/2) * zzs$coefficients[,secol]
    ci <- zzs$coefficients[,1] + cbind(ci,-ci)
    dimnames(ci)[[2]] <- paste(format(100*conf.level),"%",c("L","H"),sep="")
    
    if (exponentiate) {
      ci <- cbind(Est=zzs$coefficients[,1],ci)
      dimnames(ci)[[2]] <- paste("e(",dimnames(ci)[[2]],")",sep="")
      ci <- exp(ci)
    }
    
    zzs$coefficients <- cbind(zzs$coefficients[,1:secol,drop=F],ci,zzs$coefficients[,-(1:secol),drop=F])
    
    
    u <- fst==lst
    u[u] <- !droppedPred
    zzs$augCoefficients[u,] <- zzs$coefficients
    ncol <- dim(zzs$augCoefficients)[2]
    zzs$augCoefficients[!u,-1] <- NA
    zzs$augCoefficients[,ncol-1] <- zzs$augCoefficients[,ncol-1,drop=F]^2
    zzs$augCoefficients[fst!=lst,] <- NA
    
    zzs$useFdstn <- useFdstn
    
    ## perform partial f-tests
    for (j in 1:length(nms)) {
      if (fst[j]!=lst[j]) {
        r <- lst[j] - fst[j] + 1
        cntrst <- matrix(0,r,dim(z$X)[2])
        cntrst[1:r,fst[j]:lst[j]] <- diag(r)
        cntrst <- cntrst[,!droppedPred,drop=F]
        cntrst <- cntrst[apply(cntrst!=0,1,any),,drop=F]
        zzs$augCoefficients[j,ncol - (1:0)] <- uWaldtest (zzs, cntrst)[1:2]
      }
    }
    dfs <- NULL
    if(length(tmplist)>0){
      termsNew <- term.labels
      j <- dim(zzs$augCoefficients)[1]+1
      oldLen <- dim(zzs$augCoefficients)[1]
      for(i in 1:length(tmplist)){
        name <- dimnames(tmplist[[i]])[[2]]
        ter <- attr(termlst[[i+1]], "term.labels")
        tmp <- sapply(strsplit(name, ".", fixed=TRUE), getn, n=2)
        ## if interaction with u terms, need to add
        terInter <- grepl(":", ter, fixed=TRUE)
        if(any(terInter)){
          tertmp <- ter[terInter]
          first <- unlist(strsplit(tertmp, ":", fixed=TRUE))[1]
          ## get interact the first one with all of the others
          which.first <- sapply(tmp, grepl, first, fixed=TRUE)
          collapseInter <- function(x,y){
            return(paste(x, y, sep=":"))
          }
          newTerms <- apply(matrix(tmp[!which.first]), 1, collapseInter, x=tmp[which.first])
          tmp <- c(tmp, newTerms)
          
        }
        
        indx2 <- grepl(" ", preds)
        indx <- apply(matrix(tmp, nrow=1), 2, function(x) x==cols)
        indx <- apply(indx, 1, sum)
        
        indx <- ifelse(indx==0, FALSE, TRUE)
        r <- sum(indx)
        cntrst <- matrix(0,r,dim(z$X)[2])
        cntrst[1:r, indx] <- diag(r)
        cntrst <- cntrst[,!droppedPred,drop=F]
        cntrst <- cntrst[apply(cntrst!=0,1,any),,drop=F]
        zzs$augCoefficients <- rbind(zzs$augCoefficients, rep(NA, dim(zzs$augCoefficients)[2]))
        dimnames(zzs$augCoefficients)[[1]][j] <- names(tmplist)[i]
        zzs$augCoefficients[j,ncol - (1:0)] <- uWaldtest (zzs, cntrst)[1:2]
        dfs <- c(dfs, r)
        j <- j+1
      }
      miss <- apply(sapply(term.labels, grepl, dimnames(zzs$augCoefficients)[[1]], fixed=TRUE), 2, sum)
      if(any(miss==0)){
        nm <- names(miss)[miss==0]
        for(i in 1:length(nm)){
          tmpNum <- sapply(names(tmplist), grepl, nm[i], fixed=TRUE)
          curNms <- names(tmplist[[which(tmpNum)]])
          tmp <- sapply(strsplit(curNms, ".", fixed=TRUE), getn, n=2)
          tmp <- c(first, apply(matrix(tmp), 1, collapseInter, x=first))
          
          indx2 <- grepl(" ", preds)
          indx <- apply(matrix(tmp, nrow=1), 2, function(x) x==cols)
          indx <- apply(indx, 1, sum)
          ## check to make sure that we have the correct nested things
          indx[1:max(which(indx==0))] <- 0
          indx <- ifelse(indx==0, FALSE, TRUE)
          r <- sum(indx)
          cntrst <- matrix(0,r,dim(z$X)[2])
          cntrst[1:r, indx] <- diag(r)
          cntrst <- cntrst[,!droppedPred,drop=F]
          cntrst <- cntrst[apply(cntrst!=0,1,any),,drop=F]
          zzs$augCoefficients <- rbind(zzs$augCoefficients, rep(NA, dim(zzs$augCoefficients)[2]))
          dimnames(zzs$augCoefficients)[[1]][j] <- nm[i]
          zzs$augCoefficients[j,ncol - (1:0)] <- uWaldtest (zzs, cntrst)[1:2]
          dfs <- c(dfs, r)
          j <- j+1
        }
      }
    }
    j <- dim(zzs$augCoefficients)[2]
    zzs$augCoefficients <- suppressWarnings(cbind(zzs$augCoefficients[,-j,drop=F],df=lst-fst+1,zzs$augCoefficients[,j,drop=F]))
    zzs$augCoefficients[,dim(zzs$augCoefficients)[2]-1] <- ifelse(is.na(zzs$augCoefficients[,dim(zzs$augCoefficients)[2]-2]), NA, zzs$augCoefficients[,dim(zzs$augCoefficients)[2]-1])
    if(length(tmplist)>0 & !is.null(dfs)){
      for(i in 1:length(dfs)){
        zzs$augCoefficients[dim(zzs$augCoefficients)[1]-round(i/2),"df"] <- tail(dfs, n=1)
        dfs <- dfs[-length(dfs)]
      }
    }
    
    ## get the multiple partial tests in the correct order
    if(length(tmplist)>0){
      tmp <- zzs$augCoefficients
      test <- termTraverse(termlst, tmp, 1:dim(zzs$augCoefficients)[1])
      zzs$augCoefficients <- test$mat
    }
    class(zzs$augCoefficients) <- "augCoefficients"
    
    zzs$fnctl <- fnctl
    coefs <- zzs$augCoefficients
    
    ## Create the "raw" and "transformed" tables
    if(dim(coefs)[2]>8 & !suppress){
      zzs$model <- coefs[,c(1,2,3,7,8,9), drop=FALSE]
      zzs$transformed <- coefs[,4:9, drop=FALSE]
    } else if (dim(coefs)[2]>8 & suppress){
      zzs$model <- coefs[,c(1,2,3,7,8,9), drop=FALSE]
      zzs$transformed <- coefs[,4:9, drop=FALSE]
    } else{
      zzs$model <- coefs
      zzs$transformed <- NA
    }
    
    if(anyRepeated){
      if(fnctl !="mean" & fnctl != "hazard"){
        zzs$model <- coefs[,c(1, 2, 6, 7, 8), drop=FALSE]
        zzs$transformed <- coefs[,c(3:8), drop=FALSE]
      }
    }
    zzs$suppress <- suppress
    zzs$coefNums <- matrix(1:length(fit$coefficients), nrow=1)
    ## Inserts a column into a matrix
    insertCol <- function(x, indx, col){
      if(length(indx)==1){
        if(indx > dim(x)[2]){
          tmp <- cbind(x, col)
        } else if (indx > 1){
          tmp <- cbind(x[,1:(indx-1)], col, x[,(indx):dim(x)[2]])
        } else {
          tmp <- cbind(col, x)
        }
        return(tmp)
      } else {
        tmp1 <- insertCol(x, indx[1], col)
        tmp2 <- insertCol(tmp1, indx[-1], col)
        return(tmp2)
      }
    }
    
    ## Add in blanks for dummy labels etc
    p <- length(zzs$coefNums)
    if(p > 2 & any(is.na(zzs$augCoefficients[,1]))){
      coefNums <- matrix(zzs$coefNums%*%insertCol(diag(p), which(is.na(zzs$augCoefficients[,1]) | duplicated(dimnames(zzs$augCoefficients)[[1]])), rep(NA, p)), ncol=1)
      zzs$coefNums <- coefNums
    } 
    ## get levels for printing
    levels <- getLevels(dimnames(zzs$augCoefficients)[[1]], zzs$coefNums, names(tmplist), term.labels, 0)
    zzs$levels <- levels
    ## Get overall robustSE F-test
    if(robustSE){
      r <- dim(zzs$coefficients)[1]-1
      r <- max(1, r)
      cntrst <- matrix(0,r,dim(zzs$coefficients)[1])
      if(r==1 & fnctl=="hazard"){
        zzs$fstatistic <- summary(fit)$waldtest
      } else {
        if(intercept | dim(cntrst)[2]>1){
          cntrst[1:r, 2:dim(cntrst)[2]] <- diag(r)
        } else{
          cntrst[1:r,] <- diag(r)
        }
        zzs$fstatistic <- uWaldtest(zzs, cntrst)[c(1,3:4)]
      }
    }
    zzs$args <- args
    zzs$anyRepeated <- anyRepeated | !is.null(attr(fit$terms, "specials")$cluster)
    return(zzs)
  }