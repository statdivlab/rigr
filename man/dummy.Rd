\name{dummy}
\alias{dummy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  %%  ~~function to do ... ~~
    Create Dummy Variables
}
\description{
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
    Creates dummy variables. 
}
\usage{
  dummy(x,subset=rep(T,length(x)),
        reference=sort(unique(x[!is.na(x)])),includeAll=F, version=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    %%     ~~Describe \code{y} here~~
      variable used to create the dummy variables.
  }
  \item{subset}{
    %%     ~~Describe \code{cluster} here~~
      a subset of the data, if desired.
  }
  \item{reference}{
    the reference value for the dummy variables to compare to.
  }
  \item{includeAll}{
    logical value indicating whether all of the dummy variables should be returned (including the reference).
  }
  \item{version}{
    if \code{TRUE}, returns the version of the function and nothing else.
  }
}

\value{
  %%  ~Describe the value returned
  A matrix containing the dummy variables.
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
  # Create a dummy variable for race
  dummy(race)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
