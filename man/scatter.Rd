\name{scatter}
\alias{scatter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Scatter Plot with Lowess Curves
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Produces a scatter plot of two variables with (possibly stratified) superimposed 
lowess smooths and least squares fitted lines.
}
\usage{
 scatter(y, x, strata=rep(1,length(y)), subset= rep(TRUE,length(y)), 
 reference=sort(unique(strata)), plotPoints=TRUE, plotLowess=TRUE, plotLSfit=FALSE, 
 legend=0.05, colors=c("black", "blue", "orange", "pink", "green", "red", 
 "cornflowerblue", "darkolivegreen", "magenta"), xJitter=TRUE, yJitter=FALSE, 
 newplot=TRUE, lty=1:6, lwd=1, log="",pch=1:25, ..., version=FALSE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{y} here~~
a numeric vector containing the values to be plotted on the y-axis.
}
  \item{x}{
%%     ~~Describe \code{x} here~~
a numeric vector containing the values to be plotted on the x-axis.
}
  \item{strata}{
%%     ~~Describe \code{strata} here~~
a vector, matrix, or list of stratification variables. Descriptive statistics will be computed within strata defined by each unique combination of the stratification variables, as well as in the combined sample. If \code{strata} is supplied, all variables must be of that same length.

}
  \item{subset}{
%%     ~~Describe \code{subset} here~~
a vector indicating a subset to be used for all descriptive statistics. If \code{subset} is supplied, all variables must be of that same length.
}
  \item{reference}{
%%     ~~Describe \code{reference} here~~
a list of the strata in the order they are to be plotted.
}
  \item{plotPoints}{
%%     ~~Describe \code{plotPoints} here~~
an indicator that points are to be plotted. A different color and point type combination will be used for each stratum. Default value is \code{TRUE}.
}
  \item{plotLowess}{
%%     ~~Describe \code{plotLowess} here~~
an indicator that lowess smoths are to be plotted. A different color and line type combination will be used for each stratum. Default value is \code{TRUE}.
}
  \item{plotLSfit}{
%%     ~~Describe \code{plotLSfit} here~~
an indicator that least squares fitted lines are to be plotted. A different color and line type combination will be used for each stratum. Default value is \code{TRUE}.
}
  \item{legend}{
%%     ~~Describe \code{legend} here~~
where to place the legend on the plot
}
  \item{colors}{
%%     ~~Describe \code{colors} here~~
a vector of colors used in plotting strata.
}
  \item{xJitter}{
%%     ~~Describe \code{xJitter} here~~
the proportion of the minimal difference between adjacent x-values divided by 8 by which plotted points are to be jittered in the x-dimension. A value of 0 implies no jittering.
}
  \item{yJitter}{
%%     ~~Describe \code{yJitter} here~~
the proportion of the minimal difference between adjacent y-values divided by 8 by which plotted points are to be jittered in the y-dimension. A value of 0 implies no jittering.
}
  \item{newplot}{
%%     ~~Describe \code{newplot} here~~
logical value indicating that the graph should be plotted on a new set of axes. Default value is \code{TRUE}.
} \item{lty, lwd, log,pch}{
plotting parameters.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
optional arguments for plotting parameters (e.g. \code{xlab}, \code{ylab}, \code{main}) that will be passed to \code{plot()}.
}
  \item{version}{
%%     ~~Describe \code{version} here~~
if \code{TRUE}, the version of the function will be returned. No other computations will be performed.
}
}
\value{
%%  ~Describe the value returned
This function produces a plot. No value is returned.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
%%  ~~who you are~~
Scott S. Emerson, M.D., Ph.D., Andrew J. Spieker, Brian D. Williamson, Travis Y. Hee Wai, and Solomon Lim}

\examples{
#- Read in and attach the data -#
mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt", header=TRUE)
attach(mri)

#- Create a scatterplot with lowess curves and least squares fitted regression lines -#
scatter(atrophy, age, ylab="Atrophy", xlab="Age")

#- Create a scatterplot stratified by age on the log scale with lowess curves and -#
#- least squares fitted regression lines -#
scatter(atrophy, age, strata=male,ylab="Atrophy", xlab="Age",log="y")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
