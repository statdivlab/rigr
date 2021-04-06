\name{lincom}
\alias{lincom}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Tests of Linear Combinations of Regression Coefficients
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Produces point estimates, interval estimates, and p values for linear combinations of regression coefficients using a \code{    uRegress} object.
}
\usage{
lincom(reg, comb, hyp=0, conf.level=.95, robustSE = TRUE, eform=reg$fnctl!="mean")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{reg}{
an object of class \code{uRegress}. 
}
  \item{comb}{
a vector or matrix containing the values of the constants which create the linear combination of the form \deqn{c_0 + c_1\beta_1 + \dots}.
} \item{hyp}{
the null hypothesis to compare the linear combination of coefficients against. The default value is \code{0}. An error will be thrown if the number of columns of this matrix are not equal to the number of coefficients in the model.
}
  \item{conf.level}{
a number between 0 and 1, indicating the desired confidence level for intervals.
}
  \item{robustSE}{
a logical value indicating whether or not to use robust standard errors in calculation. If \code{TRUE}, then \code{robustSE} must have been \code{TRUE} when \code{reg} was created.
}
\item{eform}{
a logical value indicating whether or not to exponentiate the estimated coefficient. By default this is performed based on the type of regression used.
}
}

\value{
%%  ~Describe the value returned
Prints a matrix with the point estimate of the linear combination of coefficients, a p-value, and confidence interval.
}

\author{
%%  ~~who you are~~
Scott S. Emerson, M.D., Ph.D., Andrew J. Spieker, Brian D. Williamson
}

\examples{
# Loading required libraries
library(survival)
library(sandwich)

# Reading in a dataset
mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt",header=TRUE)
attach(mri)

# Linear regression of LDL on age (with robust SE by default)
testReg <- regress ("mean", ldl~age+stroke)

# Testing coefficient created by .5*age - stroke (the first 1 comes from including the intercept)
testC <- c(1, .5, -1)
lincom(testReg, testC)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
