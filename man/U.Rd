\name{U}
\alias{U}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Create a Transformed Variable
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Creates a transformed variable using either the natural log, a dummy transformation, linear splines, or a polynomial. Mostly for use in regression. If a partial formula of the form \code{~var1 + var2} is entered, returns the formula for use in regression. The partial formula can be named by adding an equals sign before the tilde.
}
\usage{
U(..., type=NULL, subset=rep(T,length(x)), knots=NULL, degree=2, 
  reference=sort(unique(x[!is.na(x)])), lbl=NULL, center=mean(x,na.rm=T), 
  includeAll=FALSE, parameterization="absolute", vrsn=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{
%%     ~~Describe \code{y} here~~
variable(s) used to create the transformation.
}
\item{type}{
a character string describing the transformation. Partial matching is used, so only enough of the string to make the transformation unique is needed.
}
\item{subset}{
used in creating dummy variables. Only used if \code{type == "dummy"}. 
}
  \item{knots}{
%%     ~~Describe \code{cluster} here~~
vector of knots to create the splines. Only used if \code{type=="lspline"}.
}
\item{degree}{
the degree of the polynomial to be returned. Only used if \code{type=="polynomial"}.
}
\item{reference}{
the reference vector for levels of the dummy variable. Only used if \code{type=="dummy"}.
}
\item{lbl}{
a label for the splines. Only used if \code{type=="lspline"}
}
\item{center}{
the center of the returned polynomial. Only used if \code{type=="polynomial"}.
}
\item{includeAll}{
a logical value to use all values even in the presense of a subset. Only used if \code{type=="dummy"}.
}
\item{parameterization}{
defaults to\code{"absolute"}, and provides splines based on the absolute slope between knots. If \code{"change"}, provides splines based on the change from knot to knot. If \code{lsplineD} is called, \code{"change"} is entered by default. Only used if \code{type=="lspline"}.
}
\item{vrsn}{
if \code{TRUE}, returns the version of the function and nothing else.
}
}

\value{
%%  ~Describe the value returned
A matrix or vector containing the transformations. The class of the returned value is \code{c("transformation", y)} where \code{y} is the class of the transformed variable (usually \code{numeric}). The type of transformation performed is encoded as one of the attributes of the returned value, along with the original data.
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
# Reading in a dataset
mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt", header=TRUE)
attach(mri)
# Create a spline based on absolute
U(ldl, type="lspline", knots=c(70, 100, 130, 160))
U(ldl, type="ls", knots=c(70,100,130,160))

# Create a spline based on change
U(ldl, type="ls", knots=c(70, 100, 130, 160), parameterization="change")

# Create a log transformed variable
U(age, type="log")

## Create a partial formula
U(ma=~male+age)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
