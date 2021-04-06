\name{polynomial}
\alias{polynomial}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  %%  ~~function to do ... ~~
    Create Polynomials
}
\description{
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
    Creates polynomial variables. 
}
\usage{
  polynomial(x,degree=2,center=mean(x,na.rm=T), version=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    %%     ~~Describe \code{y} here~~
      variable used to create the polynomials.
  }
  \item{degree}{
    %%     ~~Describe \code{cluster} here~~
      the maximum degree polynomial to be returned. Polynomials of degree <= \code{degree} will be returned.
  }
  \item{center}{
    the value to center the polynomials at.
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
  # Create a polynomial on ldl
  polynomial(ldl, degree=3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
