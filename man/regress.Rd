\name{regress}
\alias{regress}
\alias{fitted.uRegress}
\alias{print.augCoefficients}
\alias{print.uRegress}
\alias{uLRtest}
\alias{uWaldtest}
\alias{termTraverse}
\alias{explode}
\alias{indentNames}
\alias{getLevels}
\alias{testList}
\alias{pasteTwo}
\alias{processTerm}
\alias{addArgs}
\alias{pasteOn}
\alias{pasteOnSpline}
\alias{pastePair}
\alias{movingSum}
\alias{myNext}
\alias{reFormatReg}
\alias{createCols}
\alias{checkNesting}
\alias{splitOnParen}
\alias{reFormat}
\alias{equal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
General Regression for an Arbitrary Functional
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Produces point estimates, interval estimates, and p values for an arbitrary functional (mean, geometric mean,
proportion, median, quantile, odds) of a variable of class \code{integer}, \code{numeric}, \code{Surv}, when regressed on an arbitrary number of covariates. Multiple Partial F-tests can be specified using the \code{\link[uwIntroStats]{U}} function.

}
\usage{
regress(fnctl, formula, data, intercept = fnctl!="hazard", 
        strata = rep(1,n),weights=rep(1,n),id=1:n,ties="efron",subset=rep(TRUE,n),
        robustSE = TRUE, conf.level = 0.95, exponentiate = fnctl!="mean",
        replaceZeroes, useFdstn = TRUE, suppress = FALSE, na.action, method = "qr", 
        model.f = TRUE, model.x = FALSE, model.y = FALSE, qr = TRUE,
        singular.ok = TRUE, contrasts = NULL, offset,control = list(...),
        init, ..., version=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fnctl}{
%%     ~~Describe \code{fnctl} here~~
a character string indicating the functional (summary measure of the distribution) for which inference is desired. Choices include \code{"mean"}, \code{"geometric mean"}, \code{"odds"}, \code{"rate"}, \code{"hazard"}. The character string may be shortened to a unique substring. Hence \code{"mea"} will suffice for \code{"mean"}.
}
  \item{formula}{
%%     ~~Describe \code{y} here~~
an object of class \code{formula} as might be passed to \code{lm}, \code{glm}, or \code{coxph}.
}

\item{data}{
a data frame, matrix, or other data structure with matching names to those entered in \code{formula}.
}
  \item{intercept}{
%%     ~~Describe \code{intercept} here~~
a logical value indicating whether a intercept exists or not.
}
  \item{strata}{
%%     ~~Describe \code{strata} here~~
vector indicating a variable to be used for stratification in proportional hazards regression.
}
  \item{weights}{
%%     ~~Describe \code{weights} here~~
vector indicating optional weights for weighted regression.

}
  \item{id}{
%%     ~~Describe \code{id} here~~
vector with ids for the variables. If any ids are repeated, runs a clustered regression.
} \item{ties}{
One of \code{"efron"} (by default), \code{"breslow"}, or \code{"exact"}. Determines the method used to handle ties in proportional hazard regression.
}
  \item{subset}{
%%     ~~Describe \code{subset} here~~
vector indicating a subset to be used for all inference.
}
  \item{robustSE}{
%%     ~~Describe \code{robustSE} here~~
a logical indicator that standard errors are to be computed using the Huber-White sandwich estimator.
}
  \item{conf.level}{
%%     ~~Describe \code{conf.level} here~~
a numeric scalar indicating the level of confidence to be used in computing confidence intervals. The default is 0.95.
}
  \item{exponentiate}{
%%     ~~Describe \code{exponentiate} here~~
a logical indicator that the regression parameters should be exponentiated. This is by default true for all functionals except the mean.
}
  \item{replaceZeroes}{
%%     ~~Describe \code{replaceZeroes} here~~
if not \code{FALSE}, this indicates a value to be used in place of zeroes when computing a geometric mean. If \code{TRUE}, a value equal to one-half the lowest nonzero value is used. If a numeric value is supplied, that value is used.
}
  \item{useFdstn}{
%%     ~~Describe \code{useFdstn} here~~
a logical indicator that the F distribution should be used for test statistics
instead of the chi squared distribution even in logistic and proportional hazard
regression models. When using the F distribution, the degrees of freedom are
taken to be the sample size minus the number of parameters, as it would be in a
linear regression model.
}
\item{suppress}{
if \code{TRUE}, and a model which requires exponentiation (for instance, regression on the geometric mean) is computed, then a table with only the exponentiated coefficients and confidence interval is returned. Otherwise, two tables are returned - one with the original unexponentiated coefficients, and one with the exponentiated coefficients.
} \item{na.action, method, model.f, model.x, model.y, qr, singular.ok, offset, contrasts, control}{
optional arguments that are passed to the functionality of \code{lm} or \code{glm}.
} \item{init}{
optional argument that are passed to the functionality of \code{coxph}.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
other arbitrary parameters.
}
  \item{version}{
%%     ~~Describe \code{version} here~~
if \code{TRUE}, returns the version of the function. No other computation is performed.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Regression models include linear regression (for the ``mean'' functional), logistic regression (for the ``odds''
functional), Poisson regression (for the ``rate'' functional).  Proportional hazards regression is currently not supported in the \code{regress} function. Objects created using the \code{\link[uwIntroStats]{U}} function can also be passed in. If the \code{\link[uwIntroStats]{U}} call involves a partial formula of the form \code{~ var1 + var2}, then \code{regress} will return a multiple-partial F-test involving \code{var1} and \code{var2}. The multiple partial tests must be the last terms specified in the model (i.e. no other predictors can follow them).

}
\value{
%%  ~Describe the value returned
An object of class uRegress is returned. Parameter estimates, confidence intervals, and p values are contained in a matrix $augCoefficients.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
%%  ~~who you are~~
Scott S. Emerson, M.D., Ph.D., Andrew J. Spieker, Brian D. Williamson, Travis Hee Wai
}

\seealso{
Functions for fitting linear models (\code{\link[stats]{lm}}), generalized linear models (\code{\link[stats]{glm}}), proportional hazards models (\code{\link[survival]{coxph}}), and generalized estimating equations (\code{\link[geepack]{geeglm}}). Also see the function to specify multiple-partial F-tests, \code{\link[uwIntroStats]{U}}.
}

\examples{
# Loading required libraries
library(survival)
library(sandwich)

# Reading in a dataset
mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt",header=TRUE)

# Creating a Surv object to reflect time to death
mri$ttodth <- Surv(mri$obstime,mri$death)

# Attaching the mri dataset
attach(mri)

# Linear regression of atrophy on age
regress("mean", atrophy~age, data=mri)

## Linear regression of atrophy on male and race and their interaction, 
## with a multiple-partial F-test on the race-age interaction
regress("mean", atrophy~ male + U(ra=~race*age), data=mri)

## Linear regression of atrophy on age, male, race (as a dummy variable), chf,
## and diabetes. There are two multiple partial F-tests and both are named
regress("mean", atrophy~age+male+U(rc=~dummy(race)+chf)+U(md=~male+diabetes), data=mri)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
