\name{predict.uRegress}
\alias{predict.uRegress}
\alias{predict}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Prediction Intervals for \code{uRegress} objects
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Produces prediction intervals for objects of class \code{uRegress}.

}
\usage{
\method{predict}{uRegress}(object,interval="prediction",level=0.95, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
an object of class \code{uRegress}. 
}\item{interval}{
Type of interval calculation
}\item{level}{
Tolerance/confidence level
}\item{...}{
other arguments to pass to the appropriate predict function for the class of \code{object$fit}. See \code{\link[survival]{predict.coxph}}, \code{\link[stats]{predict.lm}}, or \code{\link[stats]{predict.glm}} for more details. Predictions are not currently implemented for objects of type \code{\link[geepack]{geeglm}}.
}

}

\value{
%%  ~Describe the value returned
Returns a matrix with the fitted value and prediction interval for the entered X.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
%%  ~~who you are~~
Scott S. Emerson, M.D., Ph.D., Andrew J. Spieker, Brian D. Williamson
}

\seealso{
\code{\link[uwIntroStats]{regress}}
}

\examples{
# Loading required libraries
library(survival)
library(sandwich)

# Reading in a dataset
mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt",header=TRUE)
attach(mri)

# Linear regression of LDL on age (with robust SE by default)
testReg <- regress ("mean", ldl~age)

# 95\% Prediction Interval for age 50
predict(testReg)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
