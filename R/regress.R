#' General Regression for an Arbitrary Functional
#' 
#' Produces point estimates, interval estimates, and p values for an arbitrary
#' functional (mean, geometric mean, proportion, odds) of a
#' variable of class \code{integer}, \code{numeric}, \code{Surv}, when
#' regressed on an arbitrary number of covariates. Multiple Partial F-tests can
#' be specified using the \code{\link[uwIntroStats]{U}} function.
#' 
#' Regression models include linear regression (for the ``mean'' functional), logistic
#' regression (for the ``odds'' functional), Poisson regression (for the
#' ``rate'' functional).  Proportional hazards regression is currently not
#' supported in the \code{regress} function. Objects created using the
#' \code{\link[uwIntroStats]{U}} function can also be passed in. If the
#' \code{\link[uwIntroStats]{U}} call involves a partial formula of the form
#' \code{~ var1 + var2}, then \code{regress} will return a multiple-partial
#' F-test involving \code{var1} and \code{var2}. The multiple partial tests
#' must be the last terms specified in the model (i.e. no other predictors can
#' follow them).
#' 
#' @aliases regress fitted.uRegress print.augCoefficients print.uRegress
#' uLRtest uWaldtest termTraverse explode indentNames getLevels testList
#' pasteTwo processTerm addArgs pasteOn pasteOnSpline pastePair movingSum
#' myNext reFormatReg createCols checkNesting splitOnParen reFormat equal
#' @param fnctl a character string indicating
#' the functional (summary measure of the distribution) for which inference is
#' desired. Choices include \code{"mean"}, \code{"geometric mean"},
#' \code{"odds"}, \code{"rate"}. 
#' @param formula an object of class
#' \code{formula} as might be passed to \code{lm}, \code{glm}, or \code{coxph}.
#' @param data a data frame, matrix, or other data structure with matching
#' names to those entered in \code{formula}.
#' @param intercept a logical value
#' indicating whether a intercept exists or not. Default value is \code{TRUE} for all 
#' functionals.
#' @param weights vector indicating
#' optional weights for weighted regression.
#' @param id vector with ids for the variables.
#' If any ids are repeated, runs a clustered regression.
#' @param subset vector indicating a subset
#' to be used for all inference.
#' @param robustSE a logical indicator
#' that standard errors are to be computed using the Huber-White sandwich
#' estimator.
#' @param conf.level a numeric scalar
#' indicating the level of confidence to be used in computing confidence
#' intervals. The default is 0.95.
#' @param exponentiate a logical
#' indicator that the regression parameters should be exponentiated. This is by
#' default true for all functionals except the mean.
#' @param replaceZeroes if not
#' \code{FALSE}, this indicates a value to be used in place of zeroes when
#' computing a geometric mean. If \code{TRUE}, a value equal to one-half the
#' lowest nonzero value is used. If a numeric value is supplied, that value is
#' used. Defaults to \code{TRUE} when \code{fnctl = "geometric mean"}. This parameter
#' is always \code{FALSE} for all other values of \code{fnctl}.
#' @param useFdstn a logical indicator
#' that the F distribution should be used for test statistics instead of the
#' chi squared distribution even in logistic regression
#' models. When using the F distribution, the degrees of freedom are taken to
#' be the sample size minus the number of parameters, as it would be in a
#' linear regression model.
#' @param suppress if \code{TRUE}, and a model which requires exponentiation
#' (for instance, regression on the geometric mean) is computed, then a table
#' with only the exponentiated coefficients and confidence interval is
#' returned. Otherwise, two tables are returned - one with the original
#' unexponentiated coefficients, and one with the exponentiated coefficients.
#' @param method the method to be used in fitting the model. The default value for
#' \code{fnctl = "mean"} and \code{fnctl = "geometric mean"} is \code{"qr"}, and the default value for
#' \code{fnctl = "odds"} and \code{fnctl = "rate"} is \code{"glm.fit"}. This argument is passed into the
#' lm() or glm() function, respectively. You may optionally specify \code{method = "model.frame"}, which
#' returns the model frame and does no fitting.
#' @param
#' na.action,model.f,model.x,model.y,qr,singular.ok,offset,contrasts,control
#' optional arguments that are passed to the functionality of \code{lm} or
#' \code{glm}.
#' @return An object of class uRegress is
#' returned. Parameter estimates, confidence intervals, and p values are
#' contained in a matrix $augCoefficients. 
#' @author Scott S. Emerson, M.D., Ph.D., Andrew J. Spieker,
#' Brian D. Williamson, Travis Hee Wai
#' @seealso Functions for fitting linear models (\code{\link[stats]{lm}}), and
#' generalized linear models (\code{\link[stats]{glm}}). Also see the function to specify
#' multiple-partial F-tests, \code{\link[uwIntroStats]{U}}.
#' @import geepack
#' @import sandwich
#' @import stats
#' @examples
#' 
#' # Loading required libraries
#' library(sandwich)
#' 
#' # Reading in a dataset
#' mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt",header=TRUE)
#' 
#' # Linear regression of atrophy on age
#' regress("mean", atrophy~age, data=mri)
#' 
#' ## Linear regression of atrophy on male and race and their interaction, 
#' ## with a multiple-partial F-test on the race-age interaction
#' regress("mean", atrophy~ male + U(ra=~race*age), data=mri)
#' 
#' ## Linear regression of atrophy on age, male, race (as a dummy variable), chf,
#' ## and diabetes. There are two multiple partial F-tests and both are named
#' regress("mean", atrophy~age+male+U(rc=~dummy(race)+chf)+U(md=~male+diabetes), data=mri)
#' 
#' 
#' @export regress
regress <- function(fnctl, formula, data,                     
                    intercept = TRUE, 
                    weights = rep(1,nrow(data)), id = 1:nrow(data), subset = rep(TRUE,nrow(data)),
                    robustSE = TRUE, conf.level = 0.95, exponentiate = fnctl != "mean",
                    replaceZeroes, useFdstn = TRUE, suppress = FALSE, na.action, method = "qr", model.f = TRUE, model.x = FALSE, model.y = FALSE, qr = TRUE,
                    singular.ok = TRUE, contrasts = NULL, offset,control = list(...), ...) {
  
  # define n
  n <- nrow(data)
  
  # throw errors if weights or subset are not the correct length
  if (length(weights) != n) stop("Response variable and weights must be of same length")
  if (length(subset) != n) stop("Response variable and subsetting variable must be of same length")
  
  # throw error if method is neither a character nor a function
  if (!is.character(method) && !is.function(method)) {
    stop("invalid 'method' argument")
  }
  
  cl <- match.call()
  fit <- NULL
  if (missing(formula)) {
    stop("You must enter a formula")
  }
  
  # ensure fnctl is one of the ones supported
  if (!(fnctl %in% c("mean", "geometric mean", "odds", "rate"))) {
    stop("unsupported functional")
  }
  
  # get family and glm vs. lm
  glm <- FALSE
  
  if (fnctl %in% c("odds", "rate")) {
    glm <- TRUE
    
    # if fnctl is "odds" or "rate" and method = "qr", internally change to method = "glm.fit"
    if (method == "qr") {
      method <- "glm.fit"
    }
    
    if (fnctl == "odds") {
      # if fnctl = "odds", then family = "binomial"
      family <- "binomial"
    } else {
      # if fnctl = "rate", then family = "poisson"
      family <- "poisson"
    }
  } else {
    
    # if fnctl = "mean" | fnctl = "geometric mean", then family = "gaussian
    family="gaussian"
    
  }
  
  # if intercept = FALSE, add a "-1" to the formula
  if (!intercept) {
    
    form <- deparse(formula)
    
    if (length(form) > 1) {
      form <- paste(form, collapse = "")
    }
    
    form <- paste(form, "-1")
    formula <- as.formula(form, env=.GlobalEnv)
  }
  
  # Set up the model matrix and formula
  cl <- match.call()
  
  # set up the model frame (mf), which will become the model matrix
  mf <- match.call(expand.dots = FALSE)
  
  # get indices for which parameters in mf correspond to the parameters in the vectors below
  m <- match(c("formula", "data", "subset", "weights", "na.action", 
               "offset"), names(mf), 0L)
  if (glm) {
    m <- match(c("formula", "data", "subset", "weights", "na.action", 
                 "etastart", "mustart", "offset"), names(mf), 0L) # what are etastart and mustart?
  }
  
  # Get the correct formula and any multiple-partial F-tests
  testlst <- testList(formula, mf, m, data)
  formula <- testlst$formula
  
  # tmplist is NULL if there are no multiple-partial F-tests specific in a U function
  # otherwise, tmplist is a named list containing dataframes with the variables listed inside 
  # each U function specified
  tmplist <- testlst$testList
  
  # get the terms list
  # a list containing all of the models we need to fit
  # if no multiple-partial F-tests are specified in a U function, this will be a named list of length 1
  # containing only the "overall" model
  termlst <- testlst$termList
  
  # change formula in mf so that U() no longer surrounds any variables
  mf$formula <- formula
  
  # Note from Taylor - I believe this is GEE stuff we can remove, but not 100% yet
  mftmp <- mf
  mfGee <- mf
  mG <- match(c("formula", "data", "subset", "weights", "na.action", 
                "offset", "id"), names(mfGee), 0L)
  mfGee <- mfGee[c(1L, mG)]
  mfGee[[1]] <- as.name("model.frame")
  mfGee <- eval(mfGee, parent.frame())
  id <- model.extract(mfGee, id)    
  
  # Evaluate the formula and get the correct returns
  
  # model.x and model.y are FALSE by default
  ret.x <- model.x
  ret.y <- model.y
  
  # m contains the indices corresponding to which parameters in mf correspond to:
  # c("formula", "data", "subset", "weights", "na.action", "offset")
  # here we reorder the input parameters according to the order of the vector above and remove
  # parameters not included in the vector above
  mf <- mf[c(1L, m)]

  # remove missing rows from our dataframe
  mf$drop.unused.levels <- TRUE
  
  # obtain mf, a dataframe containing only the necessary variables for our overall model
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  
  # if method == "model.frame", return the model frame, just as lm or glm would do
  if (method == "model.frame") {
    
    return(mf)
    
  } else if (!glm) {
    
    # if we're fitting a linear model, the method must be "qr". Throw a warning if other method specified 
    if (method != "qr") {
      warning(gettextf("method = '%s' is not supported. Using 'qr'", 
                       method), domain = NA)
    }
    
    # get terms from model frame
    mt <- attr(mf, "terms")

    factorMat <- NULL
    term.labels <- NULL
    
    # if anything in the formula argument was specified in a U function, length(tmplist)>0
    if (length(tmplist)>0) {
      
      # get factors and term labels from the model terms object
      # factorMat is a (p+1) x p matrix, where p is the number of predictors in the overall
      # model, and the first row of factorMat is for the outcome variable. This should be
      # equivalent to rbind(0, diag(p))
      # term.labels is a vector containing strings for each predictor in the overall model, length p
      factorMat <- attr(mt, "factors")
      term.labels <- attr(mt, "term.labels")
      
      # for each multiple-partial F-test...
      for (i in 1:length(tmplist)) {
        
        # tmpFactors is an identity matrix, with names according to the predictors for this
        # multiple-partial F-test
        tmpFactors <- attr(attr(tmplist[[i]], "terms"), "factors")

        # tmpVec is actually a matrix of 1s with one column and number of rows equal to 
        # the number of predictors for this multiple-partial F-test
        tmpVec <- as.matrix(apply(tmpFactors, 1, sum))
        
        # tmpCol is a 1x(p+1) matrix of 0s and 1s, where a 1 indicates that the variable
        # in factorMat is involved in the multiple-partial F-test
        tmpCol <- matrix(rep(0, dim(factorMat)[1]))
        tmpCol[match(dimnames(tmpVec)[[1]], dimnames(factorMat)[[1]]),] <- tmpVec
        dimnames(tmpCol) <- list(dimnames(factorMat)[[1]], names(tmplist)[[i]])
        
        # add tmpCol into factorMat
        factorMat <- cbind(factorMat, tmpCol)
        
        # get term labels for this multiple-partial F-test
        # this is the same as rownames(tmpFactors) or colnames(tmpFactors)
        # i+1 because the first item in termlst is the overall model
        ter <- attr(termlst[[i+1]], "term.labels")
        
        # Note from Taylor - I believe this if statement goes through if someone specified 
        # something like U(~a + U(~b + c))
        # but need to double check
        hasU <- grepl("U", ter)
        if (any(hasU)) {
          tmpter <- ter[hasU] 
          tmpEqter <- tmpter[grepl("=", tmpter, fixed=TRUE)]
          tmpEqter <- unlist(strsplit(tmpEqter, "U", fixed=TRUE))
          if (sum(grepl(")", tmpEqter, fixed=TRUE))!=length(tmpEqter)-1) {
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
        
        # if there are any new variables specified inside of this U() specification, add them into term.labels
        term.labels <- c(term.labels, ter)
        term.labels <- unique(term.labels)
        
        # change column names of the dataframe for thus U() specification to include the U() specification
        # before the variable names
        dimnames(tmplist[[i]])[[2]] <- paste(names(tmplist)[[i]], dimnames(tmplist[[i]])[[2]], sep=".")
      }
    }

    y <- model.response(mf, "numeric")
    
    # checking response variable y, and replacing zeroes if needed/desired for
    # geometric mean fnctl
    if (fnctl == "geometric mean") {
      
      if (missing(replaceZeroes)) {
        replaceZeroes <- min(y[y>0]/2)
        
      } else if (!replaceZeroes) {
        # if fnctl == "geometric mean" and !replaceZeroes, throw an error if y contains any zeroes
        stop("replaceZeroes cannot be false if fnctl = 'geometric mean' and y contains any zeroes")
      } else if (isTRUE(replaceZeroes)) {
        replaceZeroes <- min(y[y>0]/2)
      }
      
      y <- log(ifelse(y<=0,replaceZeroes,y))
    } else {
      # if replaceZeroes is either TRUE or a numeric value, give a warning that it will do nothing
      if (!missing(replaceZeroes)) {
        if (!replaceZeroes) {
          replaceZeroes <- NA
        } else {
          warning("replaceZeroes does not do anything for this fnctl, zeroes will not be replaced")
          replaceZeroes <- NA
        }
      }
      replaceZeroes <- NA
    }
    
    # reassign n, now that missing values have been removed
    n <- length(y)
    msng <- FALSE
    
    # if weights is not numeric, throw an error
    w <- as.vector(model.weights(mf))
    if (!is.null(w) && !is.numeric(w)) {
      stop("'weights' must be a numeric vector")
    }
    if (!is.null(w) && any(w < 0)) {
      stop("negative weights not allowed")
    }
    
    # Note from Taylor: this error message should happen earlier, and also y doesn't have rows
    offset <- as.vector(model.offset(mf))
    if (!is.null(offset)) {
      if (length(offset) != nrow(y)) {
        stop(gettextf("number of offsets is %d, should equal %d (number of observations)", 
                      length(offset), nrow(y)), domain = NA)
      }
    }
    
    # Note from Taylor: not sure when the model would be empty, is this needed?
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
      # get model matrix 
      # Note to Taylor: figure out what contrasts actually does
      x <- model.matrix(mt, mf, contrasts)
      
      # fit the overall linear model, weighted if weights are specified
      if (is.null(w)) {
        z <- lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...)
      } else {
        z <- lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok, ...)
      }
      
    }
    
    # add class, various attributes, and other values to the regression object
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
    
    # assign regression output to fit object
    fit <- z
    
    # If we're fitting a glm...
  } else {
    
    # set replaceZeroes = NA, since we only use this parameter when fnctl = "geometric mean"
    replaceZeroes <- NA
    
    # get family 
    if (is.character(family)) 
      family <- get(family, mode = "function", envir = parent.frame())
    if (is.function(family)) 
      family <- family()
    if (is.null(family$family)) {
      print(family)
      stop("'family' not recognized")
    }
    
    # set control parameter not specified, set it to glm.control(...)
    if (missing(control)) {
      control <- glm.control(...)
    }
    
    # if method is "glm.fit", then set control to glm.control(...)
    if (identical(method, "glm.fit")) {
      control <- do.call("glm.control", control)
    }
    
    # get terms from model frame
    mt <- attr(mf, "terms")
    
    factorMat <- NULL
    term.labels <- NULL
    
    # if anything in the formula argument was specified in a U function, length(tmplist)>0
    if (length(tmplist)>0) {
      
      # get factors and term labels from the model terms object
      # factorMat is a (p+1) x p matrix, where p is the number of predictors in the overall
      # model, and the first row of factorMat is for the outcome variable. This should be
      # equivalent to rbind(0, diag(p))
      # term.labels is a vector containing strings for each predictor in the overall model, length p
      factorMat <- attr(mt, "factors")
      term.labels <- attr(mt, "term.labels")
      
      # for each multiple-partial F-test...
      for (i in 1:length(tmplist)) {
        
        # tmpFactors is an identity matrix, with names according to the predictors for this
        # multiple-partial F-test
        tmpFactors <- attr(attr(tmplist[[i]], "terms"), "factors")
        
        # tmpVec is actually a matrix of 1s with one column and number of rows equal to 
        # the number of predictors for this multiple-partial F-test
        tmpVec <- as.matrix(apply(tmpFactors, 1, sum))
        
        # tmpCol is a 1x(p+1) matrix of 0s and 1s, where a 1 indicates that the variable
        # in factorMat is involved in the multiple-partial F-test
        tmpCol <- matrix(rep(0, dim(factorMat)[1]))
        tmpCol[match(dimnames(tmpVec)[[1]], dimnames(factorMat)[[1]]),] <- tmpVec
        dimnames(tmpCol) <- list(dimnames(factorMat)[[1]], names(tmplist)[[i]])
        
        # add tmpCol into factorMat
        factorMat <- cbind(factorMat, tmpCol)
        
        # get term labels for this multiple-partial F-test
        # this is the same as rownames(tmpFactors) or colnames(tmpFactors)
        # i+1 because the first item in termlst is the overall model
        ter <- attr(termlst[[i+1]], "term.labels")
        
        # Note from Taylor - I believe this if statement goes through if someone specified 
        # something like U(~a + U(~b + c))
        # but need to double check
        hasU <- grepl("U", ter)
        if (any(hasU)) {
          tmpter <- ter[hasU] 
          tmpEqter <- tmpter[grepl("=", tmpter, fixed=TRUE)]
          tmpEqter <- unlist(strsplit(tmpEqter, "U", fixed=TRUE))
          # Note from Taylor: this is slightly different than how this is handled in an lm() call,
          # there's an additional if statement right here for lm's
          tmpEqter <- as.matrix(tmpEqter)
          tmpEqter <- apply(tmpEqter, 1, splitOnParen)
          tmpEqter <- unlist(lapply(strsplit(tmpEqter, "=", fixed=TRUE), function(x) return(x[1])))
          tmpEqter[is.na(tmpEqter)] <- ""
          tmpter[grepl("=", tmpter, fixed=TRUE)] <- pasteTwo(tmpEqter)
          ter[hasU] <- tmpter
        }
        
        # if there are any new variables specified inside of this U() specification, add them into term.labels
        term.labels <- c(term.labels, ter)
        term.labels <- unique(term.labels)
        
        # change column names of the dataframe for thus U() specification to include the U() specification
        # before the variable names
        dimnames(tmplist[[i]])[[2]] <- paste(names(tmplist)[[i]], dimnames(tmplist[[i]])[[2]],sep=".")
      }
    }
    
    # Note that y here is different than in the lm call, where here we don't need the model.response to be
    # numeric
    y <- model.response(mf, "any")
    
    # Note from Taylor: I think this can be deleted, I'm not sure when length(dim(y)) would ever be 1
    if (length(dim(y)) == 1L) {
      nm <- rownames(y)
      dim(y) <- NULL
      if (!is.null(nm)) 
        names(y) <- nm
    }
    
    # assign to x, again not sure when mt would ever be an empty model
    if (!is.empty.model(mt)) {
      x <- model.matrix(mt, mf, contrasts)
    } else {
      x <- matrix(, nrow(y), 0L)
    }
    
    # reassign n, now that missing values have been removed
    n <- length(y)
    msng <- FALSE
    
    # if weights is not numeric, throw an error
    # Note from Taylor: can move this to beginning of function
    w <- as.vector(model.weights(mf))
    if (!is.null(w) && !is.numeric(w)) {
      stop("'weights' must be a numeric vector")
    }
    if (!is.null(w) && any(w < 0)) {
      stop("negative weights not allowed")
    }
      
    # Note from Taylor: this error message should happen earlier, and also y doesn't have rows
    offset <- as.vector(model.offset(mf))
    if (!is.null(offset)) {
      if (length(offset) != NROW(y)) 
        stop(gettextf("number of offsets is %d should equal %d (number of observations)", 
                      length(offset), NROW(y)), domain = NA)
    }
    
    # get starting values for glm fit
    start <- model.extract(mf, "start")
    mustart <- model.extract(mf, "mustart")
    etastart <- model.extract(mf, "etastart")
    
    # fit the overall glm
    fit <- eval(call(if (is.function(method)) "method" else method, 
                     x = x, y = y, weights = w, start = start, etastart = etastart, 
                     mustart = mustart, offset = offset, family = family, 
                     control = control, intercept = attr(mt, "intercept") > 0L))
    
    # if there is an offset and intercept in the model, refit the glm
    if (length(offset) && attr(mt, "intercept") > 0L) {
      fit2 <- eval(call(if (is.function(method)) "method" else method, 
                        x = x[, "(Intercept)", drop = FALSE], y = y, weights = w, 
                        offset = offset, family = family, control = control, 
                        intercept = TRUE))
      
      # if the model didn't converge, throw a warning
      # Note from Taylor - not sure how best to make a unit test for this warning message
      if (!fit2$converged) {
        warning("fitting to calculate the null deviance did not converge -- increase 'maxit'?")
      }
       
      # assign the deviance from this model to the original model fit 
      fit$null.deviance <- fit2$deviance
    }
    
    # add class, various attributes, and other values to the regression object
    if (model.f) 
      fit$model <- mf
    fit$na.action <- attr(mf, "na.action")
    if (model.x) 
      fit$x <- x
    if (!model.y) 
      fit$y <- NULL
    fit <- c(fit, list(call = call, formula = formula, terms = mt, 
                       data = data, offset = offset, control = control, method = method, 
                       contrasts = attr(x, "contrasts"), xlevels = .getXlevels(mt, mf)))
    class(fit) <- c(fit$class, c("glm", "lm"))
  }
  
  # model contains the X matrix (intercept + covariates) for the overall model
  model <- x
  
  # Now we have fit (regression output) and model (dataframe of predictors, X matrix)
  # First check to see if there are any repeated ids;
  # then we use geeglm or coxph with clusters
  # Note from Taylor: a lot of this can likely be removed because gee's no longer supported
  if(is.null(id)){
    id <- 1:n
  }
  anyRepeated <- FALSE
  
  # Now we build the augmented coefficients matrix
  # This is different from the regular coefficients matrix if there are multiple-partial f-tests done
  # or if there are categorical variables with more than 2 levels present in the predictors
  
  # Format terms and get the correct ordering for the augmented Coefficients matrix
  # z is overwritten here if fnctl = "mean" or "geometric mean", 
  # since earlier we assigned z to the "fit" object in the lm chunk of code
  
  z <- list(call=cl, terms=NULL, firstPred=NULL, lastPred=NULL, preds=NULL, X=NULL)
  terms <- names(fit$coefficients) 
  terms <- terms[terms != "(Intercept)"]
  
  # determine if any variables are categorical with more than two categories
  # if the coefficient names and term.labels are of different lengths, then this is the case
  terms_fromlabels <- attr(fit$terms, "term.labels")
  if (length(terms) != length(terms_fromlabels)) {
    terms_names <- terms
    terms <- terms_fromlabels
  }
  
  # Note from Taylor: I believe model is already model.matrix(fit), so this may be able to be deleted
  model <- model.matrix(fit)
  
  # get predictor variables (includes intercept if there is one)
  preds <- dimnames(model)[[2]]
  preds1 <- preds
  
  # get names of all variables in the model frame
  hyperpreds <- dimnames(mf)[[2]]
  
  # are any of the variables interactions?
  interact <- grepl(":", preds, fixed=TRUE)
  
  # create a list of length 2, containing preds (should be the same preds as before) and args
  # preds is a vector of strings, containing all predictor variables in the overall model
  # args is a list of length(hyperpreds), and each element in the list is simply one of the hyperpreds
  # Note from Taylor: based on reFormatReg(), this may be more complex if any variables are
  # specified using lspline(), dummy(), or polynomial(). this is currently untested.
  prList <- reFormatReg(preds, hyperpreds, mf)
  preds <- prList$preds
  args <- prList$args
  
  # reassign column names to model matrix, unclear why this is necessary
  dimnames(model)[[2]] <- preds

  # get column names for model matrix
  cols <- matrix(preds1, nrow=1)
  cols <- apply(cols, 2, createCols, terms)
  if(intercept) {
    cols[1] <- "(Intercept)"
  }
  names(fit$coefficients) <- preds
  
  # get number of predictors in overall model
  p <- length(terms)

  # process terms and get correct order for partial F-tests

  # need versions of terms and colnames(model) without parentheses for grep
  terms_noparens <- gsub("\\)", "", gsub("\\(", "", terms))
  colnames_model_noparens <- gsub("\\)", "", gsub("\\(", "", colnames(model)))
  
  for (i in 1:p) {
    # which columns does this term correspond to
    which_cols <- grep(terms_noparens[i], colnames_model_noparens)
    
    # if this term isn't an interaction, don't pick up the colnames(model) with a ":" in them
    if (!grepl(":", terms_noparens[i])) {
      which_inter <- grep(":", colnames_model_noparens)
      which_cols <- which_cols[!(which_cols %in% which_inter)]
    } 
    
    z <- processTerm(z, model[, which_cols], terms[i])
  }
  
  # remove "as." from before variables, if any are specified as as.integeter(variable) or
  # as.factor(variable), etc.
  tmp <- gsub("as.","",z$preds)

  # reassign predictor names to z
  z$preds <- unlist(tmp)    
  
  z$X <- model
  model <- c(z, list(y = y, strata = rep(1,n), weights = weights, id = id, subset = subset))
  z <- c(model, 
         list(fnctl=fnctl, intercept=intercept, exponentiate=exponentiate, 
              replaceZeroes=replaceZeroes, conf.level=conf.level, 
              useFdstn=useFdstn, original=model))
  
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
  
  augCoefficients <- matrix(0, sum(z$firstPred!=z$lastPred)+length(z$pred)+intercept, length(cinames))
  dimnames(augCoefficients) <- list(nms,cinames)
  
  ## Getting the correct coefficients, robustSE standard errors, and f-statistics if needed
  
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
  if(!anyRepeated) {
    if (robustSE) {
      m <- sandwich::sandwich(fit,adjust=T)
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
  
  
  
  p <- dim(zzs$coefficients)[1]
  
  if (!anyRepeated) {
    if (useFdstn) {
      waldStat <- c(waldStat,1-pf(waldStat,p-intercept,n-p),p-intercept,n-p)
      LRStat <- c(LRStat,1-pf(LRStat,p-intercept,n-p),p-intercept,n-p)
      if (!is.null(scoreStat)) scoreStat <- c(scoreStat,1-pf(scoreStat,p-intercept,n-p),p-intercept,n-p)
      
      # change p-value to robust p-value
      zzs$coefficients[,secol+2] <- 2 * pt(- abs(zzs$coefficients[,secol+1]),df=n-p)
      
      # change label for p-value from Pr(>|z|) to Pr(>|t|), and "z value" to "t value"
      colnames(zzs$coefficients)[secol+1] <- "t value"
      colnames(zzs$coefficients)[secol+2] <- "Pr(>|t|)"
      
    } else {
      waldStat <- c(waldStat,1-pchisq(waldStat,p-intercept),p-intercept)
      LRStat <- c(LRStat,1-pchisq(LRStat,p-intercept),p-intercept)
      if (!is.null(scoreStat)) scoreStat <- c(scoreStat,1-pchisq(waldStat,p-intercept),p-intercept)
      
      # change p-value to robust p-value
      zzs$coefficients[,secol+2] <- 2 * pnorm(- abs(zzs$coefficients[,secol+1]))
    }
  } 
  
  droppedPred <- is.na(fit$coefficients)
  if (anyRepeated) {
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
  
  
  u <- fst == lst
  u[u] <- !droppedPred 
 
  # assign all of coefficients matrix to relevant rows of augCoefficients
  zzs$augCoefficients[u,] <- zzs$coefficients
  
  # Note from Taylor: bad practice to assign something to a base function name
  ncol <- dim(zzs$augCoefficients)[2]
  zzs$augCoefficients[!u,-1] <- NA # Note from Taylor: I don't think this line of code actually does anything
  
  # compute f-stat as t-stat^2
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
  if (length(tmplist) > 0) {
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
    if (any(miss==0) ){
      nm <- names(miss)[miss==0]
      for (i in 1:length(nm)) {
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
  if (length(tmplist)>0 & !is.null(dfs)) {
    for (i in 1:length(dfs)) {
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
  
  if (anyRepeated) {
    if(fnctl !="mean"){
      zzs$model <- coefs[,c(1, 2, 6, 7, 8), drop=FALSE]
      zzs$transformed <- coefs[,c(3:8), drop=FALSE]
    }
  }
  zzs$suppress <- suppress
  zzs$coefNums <- matrix(1:length(fit$coefficients), nrow=1)
  
  ## Add in blanks for dummy labels etc
  p <- length(zzs$coefNums)
  if(p > 2 & any(is.na(zzs$augCoefficients[,1]))){
    coefNums <- matrix(zzs$coefNums %*% insertCol(diag(p), which(is.na(zzs$augCoefficients[,1])), rep(NA, p)), ncol=1)
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
    
    if(intercept | dim(cntrst)[2]>1){
      cntrst[1:r, 2:dim(cntrst)[2]] <- diag(r)
    } else{
      cntrst[1:r,] <- diag(r)
    }
    zzs$fstatistic <- uWaldtest(zzs, cntrst)[c(1,3:4)]
    
  }
  zzs$args <- args
  zzs$anyRepeated <- anyRepeated | !is.null(attr(fit$terms, "specials")$cluster)
  return(zzs)
}