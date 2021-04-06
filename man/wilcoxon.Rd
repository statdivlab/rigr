\name{wilcoxon}
\alias{wilcoxon}
\alias{wilcoxon.do}
\alias{print.wilcoxon}
\alias{wilcoxon.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Wilcoxon Signed Rank and Mann-Whitney-Wilcoxon Rank Sum Test
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Performs Wilcoxon signed rank test or Mann-Whitney-Wilcoxon rank sum test depending on data and logicals entered. Relies heavily on the function \code{\link[stats]{wilcox.test}}. Adds formatting and variances, and prints the z-score and p-value in addition to the test statistic and p-value.
}
\usage{
## S3 method for class 'wilcoxon'
wilcoxon(y, x=NULL, alternative="two.sided", mu=0, paired=FALSE, 
        exact=NULL, correct=FALSE, conf.int=FALSE, conf.level=0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{y} here~~
numeric vector of data values. Non-finite (missing or infinite) values will be omitted.
}
  \item{x}{
%%     ~~Describe \code{\dots} here~~
optional numeric vector of data values. Non-finite (missing or infinite) values will be omitted.
}
  \item{alternative}{
%%     ~~Describe \code{\dots} here~~
specifies the alternative hypothesis for the test; acceptable values are \code{"two.sided"}, \code{"greater"}, or \code{"less"}.
}
  \item{mu}{
%%     ~~Describe \code{\dots} here~~
the value of the null hypothesis.
}
  \item{paired}{
%%     ~~Describe \code{\dots} here~~
logical indicating whether the data are paired or not. Default is \code{FALSE}. If \code{TRUE}, data must be the same length.
}
  \item{exact}{
%%     ~~Describe \code{\dots} here~~
logical value indicating whether or not an exact test should be computed.
}
  \item{correct}{
%%     ~~Describe \code{\dots} here~~
logical indicating whether or not a continuity correction should be used and displayed.
}
  \item{conf.int}{
%%     ~~Describe \code{\dots} here~~
logical indicating whether or not to calculate and display a 'confidence interval' (performs a semi-parametric test on medians, and is non-robust) and point estimate.
}
  \item{conf.level}{
%%     ~~Describe \code{\dots} here~~
confidence level for the interval.
} \item{\dots}{
only used in the generic S3 class.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
See above.
}
\value{
%%  ~Describe the value returned
A list with class \code{"wilcoxon"} is returned. The print method lays out the information in an easy to read format.
%%  If it is a LIST, use
\item{statistic}{the value of the test statistic with a name describing it.}
\item{parameter}{the parameter(s) for the exact distribution of the test statistic.}
\item{p.value}{the p-value for the test (calculated for the test statistic).}
\item{null.value}{the parameter \code{mu}.}
\item{alternative}{character string describing the alternative hypothesis.}
\item{method}{the type of test applied.}
\item{data.name}{a character string giving the names of the data.}
\item{conf.int}{a confidence interval for the location parameter (only present if the argument \code{conf.int=TRUE}).}
\item{estimate}{an estimate of the location parameter (only present if the argument \code{conf.int=TRUE}).}
\item{table}{a formatted table of rank sum and number of observation values, for printing.}
\item{vars}{a formatted table of variances, for printing.}
\item{hyps}{a formatted table of the hypotheses, for printing.}
\item{inf}{a formatted table of inference values, for printing.}

%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
%%  ~~who you are~~
Scott S. Emerson, M.D., Ph.D., Andrew J. Spieker, Brian D. Williamson
}
\seealso{
\code{\link[stats]{wilcox.test}}
}
\examples{
#- Create the data -#
cf <- c(1153, 1132, 1165, 1460, 1162, 1493, 1358, 1453, 1185, 1824, 1793, 1930, 2075)
healthy <- c(996, 1080, 1182, 1452, 1634, 1619, 1140, 1123, 1113, 1463, 1632, 1614, 1836)

#- Perform the test -#
wilcoxon(cf, healthy, paired=TRUE)

#- Perform the test -#
wilcoxon(cf, healthy, conf.int=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
