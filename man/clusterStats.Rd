\name{clusterStats}
\alias{clusterStats}
\alias{clusterStatsOld}
\alias{extract.tableStat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Summary Measures within Clusters
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Produces a vector containing summary measures computed within clusters.
}
\usage{
clusterStats(y, cluster = NULL, stat = "count", subset = NULL, x = NULL, ..., 
             version = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{y} here~~
a vector, \code{Date}, or \code{Surv} object for which within cluster summary statistics are desired.
}
  \item{cluster}{
%%     ~~Describe \code{cluster} here~~
vector, matrix, or list of variables defining clusters. Descriptive statistics will be
computed within strata defined by each unique combination of the cluster
variables.

}
  \item{stat}{
%%     ~~Describe \code{stat} here~~
a character string indicating the descriptive statistic(s) to be returned for each
cluster. See the documentation for \code{tableStat()} for a full description, although
only single statistics can be specified in this function. If either \code{"probabilities"}
or \code{"quantiles"} are specified, only the first such quantity is returned. In addition
to the summary statistics allowed by \code{tableStat()}, a user can also specify within
cluster least squares slopes (\code{stat="slope"}) of \code{y} on \code{x}.

}
  \item{subset}{
%%     ~~Describe \code{subset} here~~
a logical vector indicating a subset to be used for all descriptive statistics.

}
  \item{x}{
%%     ~~Describe \code{x} here~~
a numeric vector to be used as regression predictor for least squares slopes.

}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
optional arguments specifying quantiles or thresholds for probabilities to be used
in calculating summary statistics. See arguments for \code{descrip()}.

}
  \item{version}{
%%     ~~Describe \code{version} here~~
if \code{TRUE}, the version of the function will be returned. No other computations will
be performed.

}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
This function uses \code{tableStat()} to compute stratified statistics for each cluster. However, only single summary measures can be used in this function. See examples.
}
\value{
%%  ~Describe the value returned
A vector is returned that contains the summary statistic relevant for the cluster to which each observation in \code{y} belings. Although only the cases indicated by \code{subset} are used to calculate the summary statistics, values are expanded out to cases beyond those indicated by \code{subset}.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
%%  ~~who you are~~
Scott S. Emerson, M.D., Ph.D., Andrew J. Spieker, Brian D. Williamson, Travis Y. Hee Wai, and Solomon Lim
}
\examples{
# Load required libraries
library(survival)

# Reading in a dataset
audio <- read.csv("http://www.emersonstatistics.com/datasets/audio.csv",header=TRUE)

# Generating counts for each subject
counts <- clusterStats (audio$R4000, audio$Subject, "count")
table(counts,strata=audio$Dose)

# Generating average R4000 for each subject
mR4000 <- clusterStats (audio$R4000, audio$Subject, "mean")
descrip(mR4000,strata=audio$Dose)

# Generating average R4000 for each subject after visit 0
mtxR4000 <- clusterStats (audio$R4000, audio$Subject, "mean", subset=audio$Visit>0)
descrip(mtxR4000,strata=audio$Dose)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
