\name{ttesti}
%- Also NEED an '\alias' for EACH other topic documented here.
\alias{ttesti}
\title{
%%  ~~function to do ... ~~
T-test Given Descriptive Statistics with Improved Layout
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Produces table of relevant descriptive statistics and inference for either one- or two-sample t-test. In the two-sample case, the user can specify whether or not equal variances should be presumed. Can also perform a test of equality of proportions, with the appropriate Wald or exact binomial based confidence intervals.
}
\usage{
## Method with the appropriate defaults filled in
ttesti(obs, mean, sd, obs2=NA, mean2=NA, sd2=NA, 
       null.hyp = 0, level=.95, alternative="two.sided", 
       var.eq = FALSE, prop=FALSE, exact=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obs}{
%%     ~~Describe \code{var1} here~~
number of observations for the first sample.
}\item{mean}{
%%     ~~Describe \code{var1} here~~
the sample mean of the first sample.
}\item{sd}{
%%     ~~Describe \code{var1} here~~
the sample standard deviation of the first sample.
}\item{obs2}{
number of observations for the second sample (this is optional).
}\item{mean2}{
if \code{obs2} is supplied, then sample mean of the second sample must be supplied.
}
\item{sd2}{
if \code{obs2} is supplied, then sample standard deviation of the second sample must be supplied.
}
\item{null.hyp}{
%%     ~~Describe \code{var1} here~~
a number specifying the null hypothesis for the mean (or difference in means if performing a two-sample test). Defaults to zero.
}\item{alternative}{
%%     ~~Describe \code{var1} here~~
a string: one of \code{"less"}, \code{"two.sided"}, or \code{"greater"} specifying the form of the test. Defaults to a two-sided test.
}\item{level}{
%%     ~~Describe \code{var1} here~~
confidence level of the test. Defaults to 95/100.
}\item{var.eq}{
%%     ~~Describe \code{var1} here~~
a logical value, either \code{TRUE} or \code{FALSE} (default), specifying whether or not equal variances should be presumed in a two-sample t-test. Also controls robust standard errors.
}
\item{prop}{
if \code{TRUE}, performs a test of equality of proportions with Wald based confidence intervals. 
}
\item{exact}{
must be \code{FALSE} if \code{prop=FALSE}. If true, performs a test of equality of proportions with Exact Binomial based confidence intervals.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Values must be placed in the specified spaces, in place of the defaults. If \code{obs2}, \code{mean2}, or \code{sd2} is specified, then all three must be specified and a two-sample t-test is run. 
}
\value{
%%  ~Describe the value returned
Prints a summary of the data and the corresponding t-test. 
%%  If it is a LIST, use
  \item{Variable}{\code{x} in a one-sample test, or \code{x} and \code{y} in a two sample test. The first set of descriptives entered goes to \code{x}.}
  \item{Obs}{Number of observations of each variable: includes missing values.}
  \item{Mean}{the sample mean; also, the estimated difference in means in a two-sample test.}
  \item{Std.Err.}{the estimated standard error of the mean and of the difference in the two-sample test.}
  \item{Std.Dev.}{standard deviation estimates.}
  \item{CI}{a confidence interval for the means, and for the difference in the two-sample test. This is at the confidence level specified in the argument. If \code{prop} and/or \code{exact} are specified, also returns the appropriate Wald or Exact Binomial based confidence interval.}
  \item{Null hypothesis}{a statement of the null hypothesis.}
  \item{Alternative hypothesis}{a statement of the alternative hypothesis.}
  \item{t}{value of the t-statistic.}
  \item{df}{the degrees of freedom for the test.}
  \item{Pr}{a p-value for inference on the corresponding hypothesis test.}

%% ...
}
\author{
%%  ~~who you are~~
Scott S. Emerson, M.D., Ph.D., Andrew J. Spieker, Brian D. Williamson, Travis Y. Hee Wai, Solomon Lim
}
\examples{
#- T-test given sample descriptives -#
ttesti(24, 175, 35, null.hyp=230)

#- Two-sample test -#
ttesti(10, -1.6, 1.5, 30, -.7, 2.1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
