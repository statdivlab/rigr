\name{tableStat}
\alias{tableStat}
\alias{tableStat.default}
\alias{tableStat.do}
\alias{print.tableStat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Table of Stratified Descriptive Statistics
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Produces a table of stratified descriptive statistics for a single variable of class \code{integer}, \code{numeric}, \code{Surv},
\code{Date}, or \code{factor}. Descriptive statistics are those that can be estimated using the \code{\link[uwIntroStats]{descrip}} function.
}
\usage{
## S3 class 'tableStat'
tableStat(variable=NULL, ..., stat="count", printer=TRUE, na.rm=TRUE, 
        subset=NULL, probs= c(.25,.50,.75), replaceZeroes=FALSE, 
        restriction=Inf, above=NULL, below=NULL, labove=NULL, rbelow=NULL, lbetween=NULL, 
        rbetween=NULL, interval=NULL, linterval=NULL, rinterval=NULL, lrinterval=NULL, 
        version=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{variable}{
%%     ~~Describe \code{variable} here~~
a vector or \code{Surv} object suitable for use as an argument to \code{descrip()}. If a \code{NULL}
value is supplied for \code{variable}, the valid statistics returned by the function is
only the cross-tabulation of counts and percentages within strata.

}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
an arbitrary number of stratification variables. The arguments can be vectors,
matrices, or lists. Individual columns of a matrix or elements of a list may be of
class \code{numeric}, \code{factor}, or \code{character}. Stratification variables must all be the
same length as each other and (if it is supplied) variable.
}
\item{stat}{
a vector of character strings indicating the descriptive statistic(s) to be tabulated
within strata. Possibilities include any statistic returned by descrip() as
specified by one or more of ``count'', ``missing'', ``mean'', ``geometric mean'',
``median'', ``sd'', ``variance'', ``minimum'', ``maximum'', ``quantiles'',
``probabilities'', ``mn(sd)'', ``range'', ``iqr'', ``all'', ``row\%'', ``col\%'', or ``tot\%''.
Only enough of the string needs to be specified to disambiguate the choice.
Alternatively (and more usefully), a single special format character string can be
specified as described in the Details below.
} \item{printer}{
a logical indicating whether or not the function should return the values necessary for a print with special characters as laid out in \code{stat}.
}
\item{na.rm}{
an indicator that missing data is to be removed prior to computation of the
descriptive statistics.
}
  \item{subset}{
%%     ~~Describe \code{subset} here~~
vector indicating a subset to be used for all descriptive statistics. If \code{subset} is supplied, all variables must be of that same length.
}
  \item{probs}{
%%     ~~Describe \code{probs} here~~
a vector of probabilities between 0 and 1 indicating quantile estimates to be included in the descriptive statistics. Default is to compute 25th, 50th (median) and 75th percentiles.
}
\item{replaceZeroes}{
%%     ~~Describe \code{restriction} here~~
if not \code{FALSE}, this indicates a value to be used in place of zeroes when computing a geometric mean. If \code{TRUE}, a value equal to one-half the lowest nonzero value is used. If a numeric value is supplied, that value is used for all variables.

}
  \item{restriction}{
%%     ~~Describe \code{restriction} here~~
a value used for computing restricted means, standard deviations, and geometric means with censored time to event data. The default value of \code{Inf} will cause restrictions at the highest observation. Note that the same value is used for all variables of class \code{Surv}.
}
  \item{above}{
%%     ~~Describe \code{above} here~~
a vector of values used to dichotomize variables. The descriptive statistics will include an estimate for each variable of the proportion of measurements with values greater than each element of \code{above}.
}
  \item{below}{
%%     ~~Describe \code{below} here~~
a vector of values used to dichotomize variables. The descriptive statistics will include an estimate for each variable of the proportion of measurements with values less than each element of \code{below}.
}
  \item{labove}{
%%     ~~Describe \code{labove} here~~
a vector of values used to dichotomize variables. The descriptive statistics will include an estimate for each variable of the proportion of measurements with values greater than or equal to each element of \code{labove}.
}
  \item{rbelow}{
%%     ~~Describe \code{rbelow} here~~
a vector of values used to dichotomize variables. The descriptive statistics will include an estimate for each variable of the proportion of measurements with values less than or equal to each element of \code{rbelow}.
}
  \item{lbetween}{
%%     ~~Describe \code{lbetween} here~~
a vector of values with \code{-Inf} and \code{Inf} appended is used as cutpoints to categorize variables. The descriptive statistics will include an estimate for each variable of the proportion of measurements with values between successive elements of \code{lbetween}, with the left hand endpoint included in each interval.
}
  \item{rbetween}{
%%     ~~Describe \code{rbetween} here~~
a vector of values with \code{-Inf} and \code{Inf} appended is used as cutpoints to categorize variables. The descriptive statistics will include an estimate for each variable of the proportion of measurements with values between successive elements of \code{rbetween}, with the right hand endpoint included in each interval.
}
  \item{interval}{
%%     ~~Describe \code{interval} here~~
a two column matrix of values in which each row is used to define intervals of interest to categorize variables. The descriptive statistics will include an estimate for each variable of the proportion of measurements with values between two elements in a row, with neither endpoint included in each interval.
}
  \item{linterval}{
%%     ~~Describe \code{linterval} here~~
a two column matrix of values in which each row is used to define intervals of interest to categorize variables. The descriptive statistics will include an estimate for each variable of the proportion of measurements with values between two elements in a row, with the left hand endpoint included in each interval.
}
  \item{rinterval}{
%%     ~~Describe \code{rinterval} here~~
a two column matrix of values in which each row is used to define intervals of interest to categorize variables. The descriptive statistics will include an estimate for each variable of the proportion of measurements with values between two elements in a row, with the right hand endpoint included in each interval.
}
  \item{lrinterval}{
%%     ~~Describe \code{lrinterval} here~~
a two column matrix of values in which each row is used to define intervals of interest to categorize variables. The descriptive statistics will include an estimate for each variable of the proportion of measurements with values between two elements in a row, with both endpoints included in each interval.
}
  \item{version}{
%%     ~~Describe \code{version} here~~
If \code{TRUE}, the version of the function will be returned. No other computations will be performed.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
This function uses \code{descrip()} to compute the descriptive statistics. In addition to the basic choices specified above for \code{stat}, the user can supply a special format character string. Arbitrary text can be specified to label any of the descriptive statistics, which are indicated by bracketing with a ``@''. All text bracketed by a ``@'' must refer to a descriptive statistic, and all other text is printed verbatim. For instance, a display of the mean, standard deviation, minimum, maximum, and sample size might be specified by ``@mean@ (@sd@; @min@ - @max@; n=@count@)''. Similarly, a cross tabulation displaying counts, row percentages, column percentages, and percentages of the total might be specified by ``@count@ (r @row\%@; c @col\%@; t @tot\%@)''. See examples for more detal. Any call to \code{tableStat()} will run \code{tableStat.default()}, with user specified values in place of the appropriate defaults.
}
\value{
%%  ~Describe the value returned
An object of class \code{tableStat} is returned, which consists of a list of arrays. Each array corresponds to a table of stratified statistics for one of the possible choices of \code{stat}. The print method provides the formatted output for the choice specified in \code{stat}.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
%%  ~~who you are~~
Scott S. Emerson, M.D., Ph.D., Andrew J. Spieker, Brian D. Williamson, Travis Y. Hee Wai, Solomin Lim
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
# Load required libraries
library(survival)

# Reading in a dataset
mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt",header=TRUE)

# Creating a Surv object to reflect time to death
mri$ttodth <- Surv(mri$obstime,mri$death)

# Reformatting an integer MMDDYY representation of date to be a Date object
mri$mridate <- as.Date(paste(trunc(mri$mridate/10000),trunc((mri$mridate \%\% 10000)/100),
mri$mridate \%\% 100,sep="/"),"\%m/\%d/\%y")

# Cross tabulation of counts with sex and race strata
with (mri, tableStat (NULL, race, male, stat= "@count@ (r @row\%@; c @col\%@; t @tot\%@)"))

# Cross tabulation of counts with sex, race, and coronary disease strata
# (Note row and column percentages are defined within the first two strata, while overall
# percentage considers all strata)
with (mri, tableStat (NULL, race, male, chd,
stat= "@count@ (r @row\%@; c @col\%@; t @tot\%@)"))

# Description of time to death with appropriate quantiles
with (mri, tableStat(ttodth,probs=c(0.05,0.1,0.15,0.2),
stat="mean @mean@ (q05: @q@; q10: @q@; q15: @q@; q20: @q@; max: @max@)"))

# Description of mridate with mean, range stratified by race and sex
with (mri, tableStat(mridate, race, male,
stat="mean @mean@ (range @min@ - @max@)"))

# Stratified descriptive statistics with proportions
with (mri, tableStat(age,stat=">75: @p@; >85: @p@; [-Inf,75): @p@; [75,85): @p@; 
      [85,Inf): @p@"), above=c(75,85),lbetween=c(75,85))

# Descriptive statistics on a subset comprised of males
with (mri, tableStat(dsst,age,stroke,subset=male==1,
stat="@mean@ (@sd@; n= @count@/@missing@)"))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
