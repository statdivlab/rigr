\name{uResiduals}
\alias{uResiduals}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Extract Residuals from uRegress objects
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Extracts residuals (unstandardized, standardized, studentized, or jackknife) from \code{uRegress} objects. 
}
\usage{
uResiduals(object, type="", version=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
  an object of class \code{uRegress}, as returned by \link[uwIntroStats]{regress}.
  }
  \item{type}{
  denotes the type of residuals to return. Default value is \code{""}, which returns unstandardized residuals. \code{"standardized"}, \code{"studentized"}, and \code{"jackknife"} return the expected type of residuals.
  }
  \item{version}{
%%     ~~Describe \code{version} here~~
if \code{TRUE}, the version of the function will be returned. No other computations will
be performed.

}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Relies on functionality from the \code{stats} package to return residuals from the \code{uRegress} object. \code{"studentized"} residuals are computed as internally studentized residuals, while \code{"jackknife"} computes the externally studentized residuals.
}
\value{
%%  ~Describe the value returned
Returns the type of residuals requested.
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
\code{\link[uwIntroStats]{regress}}, \code{\link[stats]{rstudent}}, \code{\link[stats]{rstandard}}
}
\examples{
# Load required libraries
library(survival)

# Reading in a dataset
mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt",header=TRUE)

# Create a uRegress object, regressing ldl on age
ldlReg <- regress("mean", age~ldl, data=mri)

# Get the studentized residuals
uResiduals(ldlReg, "studentized")

# Get the jackknifed residuals
uResiduals(ldlReg, "jackknife")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
