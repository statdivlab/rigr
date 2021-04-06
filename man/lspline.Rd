\name{lspline}
\alias{lspline}
\alias{lsplineD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Create Linear Splines
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Creates linear splines, mostly for use in regression. 
}
\usage{
lspline(x, knots, lbl=NULL, 
        parameterization="absolute", version=FALSE)
lsplineD(x, knots, lbl=NULL, version=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{y} here~~
variable used to create the linear splines.
}
  \item{knots}{
%%     ~~Describe \code{cluster} here~~
vector of knots to create the splines.
}
\item{lbl}{
a label for the splines.
}
\item{parameterization}{
defaults to\code{"absolute"}, and provides splines based on the absolute slope between knots. If \code{"change"}, provides splines based on the change from knot to knot. If \code{lsplineD} is called, \code{"change"} is entered by default.
}
\item{version}{
if \code{TRUE}, returns the version of the function and nothing else.
}
}

\value{
%%  ~Describe the value returned
A matrix containing the linear splines.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
%%  ~~who you are~~
Scott S. Emerson, M.D., Ph.D., Andrew J. Spieker, Brian D. Williamson
}
\examples{
# Reading in a dataset
mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt", header=TRUE)
attach(mri)
# Create a spline based on absolute
lspline(ldl, c(70, 100, 130, 160))

# Create a spline based on change
lsplineD(ldl, c(70, 100, 130, 160))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
