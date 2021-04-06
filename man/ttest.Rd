\name{ttest}
\alias{ttest}
\alias{ttest.do}
\alias{plot.ttest}
\alias{print.ttest}
\alias{ttest.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
T-test with Improved Layout
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Produces table of relevant descriptive statistics and inference for either one- or two-sample t-test. In the two-sample case, the user can specify whether or not observations are matched, and whether or not equal variances should be presumed. Can also perform a test of equality of proportions (Wald based or exact binomial based).
}
\usage{
## S3 method for class "ttest"
ttest(var1, var2 = NA, by = NA, strat=NULL, geom=FALSE, prop=FALSE,
        exact=FALSE, null.hypoth = 0, test.type = "two.sided", 
        var.eq = FALSE, conf.level = 0.95, matched = FALSE, more.digits = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var1}{
%%     ~~Describe \code{var1} here~~
a (non-empty) numeric vector of data values.
}\item{var2}{
%%     ~~Describe \code{var1} here~~
an optional (non-empty) numeric vector of data.
}\item{by}{
%%     ~~Describe \code{var1} here~~
a variable of equal length to that of \code{var1} with two outcomes. This will be used to define strata for a t-test on \code{var1}.
}\item{strat}{
a variable to use instead of \code{by}. However, using \code{by} instead is recommended.
}\item{geom}{
a logical indicating whether the geometric mean should be calculated and displayed.
}
\item{prop}{
if \code{TRUE}, performs a test of equality of proportions with Wald based confidence intervals. 
}
\item{exact}{
must be \code{FALSE} if \code{prop=FALSE}. If true, performs a test of equality of proportions with Exact Binomial based confidence intervals.
}
\item{null.hypoth}{
%%     ~~Describe \code{var1} here~~
a number specifying the null hypothesis for the mean (or difference in means if performing a two-sample test). Defaults to zero.
}\item{test.type}{
%%     ~~Describe \code{var1} here~~
a string: one of \code{"less"}, \code{"two.sided"}, or \code{"greater"} specifying the form of the test. Defaults to a two-sided test.
}\item{var.eq}{
%%     ~~Describe \code{var1} here~~
a logical value, either \code{TRUE} or \code{FALSE} (default), specifying whether or not equal variances should be presumed in a two-sample t-test. Also controls robust standard errors.
}\item{conf.level}{
%%     ~~Describe \code{var1} here~~
confidence level of the test. Defaults to 95/100.
}\item{matched}{
%%     ~~Describe \code{var1} here~~
a logical value, either \code{TRUE} or \code{FALSE}, indicating whether or not the variables of a two-sample t-test are matched. Variables must be of equal length.
}\item{more.digits}{
%%     ~~Describe \code{var1} here~~
a numeric value specifying whether or not to display more or fewer digits in the output. Non-integers are automatically rounded down. Any call to \code{ttest()} will run \code{ttest.default()}, with user specified values in place of the appropriate defaults.
}\item{\dots}{
only used in the generic S3 class.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Missing values must be given by \code{"NA"}s to be recognized as missing values. Any call to \code{ttest()} is run by \code{ttest.default()}, with user specified values in place of defaults in the appropriate places.
}
\value{
%%  ~Describe the value returned
Prints a summary of the data and the corresponding t-test. 
%%  If it is a LIST, use
  \item{Variable}{the variable name supplied to the t-test function}
  \item{Group}{the group name: either the variable names supplied to the function or the names of the strata if the variable \code{by} was specified.}
  \item{Obs}{Number of observations of each variable: includes missing values.}
  \item{Missing}{number of missing values in each data vector.}
  \item{Mean}{the sample mean of each data vector; also, the estimated difference in means in a two-sample test.}
  \item{Std.Err.}{the estimated standard error of the mean and of the difference in the two-sample test.}
  \item{Std.Dev.}{standard deviation estimates from the data.}
  \item{CI}{a confidence interval for the means, and for the difference in the two-sample test. This is at the confidence level specified in the argument. If \code{prop} and/or \code{exact} are \code{TRUE}, also returns the appropriate confidence interval for the test of equality of proportions.}
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
\seealso{
\code{\link[stats]{t.test}}
}
\examples{
#- Read in data set -#
psa <- read.table("http://www.emersonstatistics.com/datasets/psa.txt", header=TRUE)
attach(psa)

#- Perform t-test -#
ttest(pretxpsa, null.hypoth = 100, test.type = "greater", more.digits = 1)

#- Define new binary variable as indicator -#
#- of whether or not bss was worst possible -#
bssworst <- bss
bssworst[bss == 1] <- 0
bssworst[bss == 2] <- 0
bssworst[bss == 3] <- 1

#- Perform t-test allowing for unequal -#
#- variances between strata -#
ttest(pretxpsa, by = bssworst)

#- Perform matched t-test -#
ttest(pretxpsa, nadirpsa, matched = TRUE, conf.level = 99/100, more.digits = 1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
