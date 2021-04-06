\name{bplot}
\alias{bplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Boxplot with Lowess Curves, Jittered Data, Overlaid Mean and Standard Deviation, for an Arbitrary Number of Strata
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function adds functionality to the base R \code{\link[graphics]{boxplot}} function. Now it is straightforward to add jittered data to the plot, and to overlay information about the sample mean and standard deviation. The function also supports stratification.
}
\usage{
bplot(y, x=rep(1, length(y)), data = NA, strata = NULL, xjitter = TRUE, yjitter = TRUE, 
      range = 0, sd = TRUE, sdx = TRUE, log = FALSE, 
      cex = c(meancex = 1, jittercex = 0.8), 
      col = c(sdcol = "dodgerblue3", jittercol = "gray30"), main = NULL, xlab = NULL, 
      ylab = NULL, names = NULL, ylim = NULL, legend=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{y} here~~
dependent variable.
}
  \item{x}{
%%     ~~Describe \code{x} here~~
independent variable. Must by the same length as \code{y}. 
}
  \item{data}{
%%     ~~Describe \code{data} here~~
if entered, must contain both \code{y} and \code{x}. 
}
  \item{strata}{
%%     ~~Describe \code{strata} here~~
strata variable(s), used to stratify the plot.
}
  \item{xjitter}{
%%     ~~Describe \code{jitter} here~~
a logical specifying if the jittered data (jittered on x-axis) are to be displayed or not. Default is \code{TRUE}.
}
  \item{yjitter}{
%%     ~~Describe \code{jitter} here~~
a logical specifying if the jittered data (jittered on y-axis) are to be displayed or not. Default is \code{TRUE}.
}
  \item{range}{
%%     ~~Describe \code{range} here~~
passed to the \code{boxplot()} function. This determines how far the plot whiskers extend out from the box. If range is positive, the whiskers extend to the most extreme data point which is no more than range times the interquartile range from the box. A value of zero causes the whiskers to extend to the data extremes.
}
  \item{sd}{
%%     ~~Describe \code{sd} here~~
a logical specifying if the standard devation of \code{y} should be overlaid on the plot. Default value is \code{TRUE}.
}
  \item{sdx}{
%%     ~~Describe \code{sdx} here~~
a logical specifying if the standard deviation of \code{x} should be overlaid on the plot. Default value is \code{TRUE}.
 }
  \item{log}{
%%     ~~Describe \code{log} here~~
a logical specifying if the data are to be displayed on a log scale. Passed to \code{boxplot()}. Default value is \code{FALSE}.
}
  \item{cex}{
%%     ~~Describe \code{cex} here~~
passed to \code{boxplot()}. 
}
  \item{col}{
%%     ~~Describe \code{col} here~~
passed to \code{boxplot()}.
}
  \item{main}{
%%     ~~Describe \code{main} here~~
passed to \code{plot()}, the main title of the plot.
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
passed to \code{plot()}, the x-axis label.

}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
passed to \code{plot()}, the y-axis label.

}
  \item{names}{
%%     ~~Describe \code{names} here~~
names (if any) of \code{x}.

}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
the range for plotting the y-axis, passed to \code{plot}.
}
  \item{legend}{
  a logical value. If \code{TRUE}, (and the means and standard deviations have been overlaid on the graph) displays a legend next to the first boxplot plotted denoting the max, mean+sd, mean, mean-sd, and min values.
  }
}

\value{
%%  ~Describe the value returned
Produces a plot. No value is returned.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
%%  ~~who you are~~
Scott S. Emerson, M.D., Ph.D., Andrew J. Spieker, Brian D. Williamson, Travis Y. Hee Wai, and Solomon Lim}

\seealso{
\code{\link[graphics]{boxplot}}
}
\examples{
#- Read in and attach the data -#
mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt", header=TRUE)
attach(mri)

#- Produce box plot with jittered data, sample mean, and sd -#
bplot(y=atrophy, x=male, xlab="Gender", ylab="Atrophy")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
