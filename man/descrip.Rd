\name{descrip}
\alias{descrip}
\alias{ifelse1}
\alias{print.uDescriptives}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Descriptive Statistics
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Produces table of relevant descriptive statistics for an arbitrary number of variables of class \code{integer}, \code{numeric}, \code{Surv}, \code{Date}, or \code{factor}. Descriptive statistics can be obtained within strata, and the user can specify that only a subset of the data be used. Descriptive statistics include the count of observations, the count of cases with missing values, the mean, standard deviation, geometric mean, minimum, and maximum. The user can specify arbitrary quantiles to be estimated, as well as specifying the estimation of proportions of observations within specified ranges. 
}
\usage{
descrip(..., strata = NULL, subset = NULL, probs = c(0.25, 0.5, 0.75), 
        geomInclude = FALSE, replaceZeroes = FALSE, restriction = Inf, above = NULL, 
        below = NULL, labove = NULL, rbelow = NULL, lbetween = NULL, rbetween = NULL, 
        interval = NULL, linterval = NULL, rinterval = NULL, lrinterval = NULL, 
        version = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
an arbitrary number of variables for which descriptive statistics are desired. The arguments can be vectors, matrices, or lists. Individual columns of a matrix or elements of a list may be of class \code{numeric}, \code{factor}, \code{Surv}, or \code{Date}. Factor variables are converted to integers. Character vectors will be coerced to numeric. Variables may be of different lengths, unless \code{strata} or \code{subset} are non-\code{NULL}.
}
  \item{strata}{
%%     ~~Describe \code{strata} here~~
vector, matrix, or list of stratification variables. Descriptive statistics will be computed within strata defined by each unique combination of the stratification variables, as well as in the combined sample. If \code{strata} is supplied, all variables must be of that same length.
}
  \item{subset}{
%%     ~~Describe \code{subset} here~~
vector indicating a subset to be used for all descriptive statistics. If \code{subset} is supplied, all variables must be of that same length.
}
  \item{probs}{
%%     ~~Describe \code{probs} here~~
a vector of probabilities between 0 and 1 indicating quantile estimates to be included in the descriptive statistics. Default is to compute 25th, 50th (median) and 75th percentiles.
}
  \item{geomInclude}{
%%     ~~Describe \code{geomInclude} here~~
If not \code{FALSE}, includes the geometric mean in the descriptive statistics. Default is \code{FALSE}.
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
This function depends on the \code{survival} R package. You should execute \code{library(survival)} if that library has not been previously installed. Quantiles are computed for uncensored data using the default method in \code{quantile()}. For variables of class \code{factor}, descriptive statistics will be computed using the integer coding for factors. For variables of class \code{Surv}, estimated proportions and quantiles will be computed from Kaplan-Meier estimates, as will be restricted means, restricted standard deviations, and restricted geometric means. For variables of class \code{Date}, estimated proportions will be labeled using the Julian date since January 1, 1970.

}
\value{
%%  ~Describe the value returned
An object of class \code{uDescriptives} is returned. Descriptive statistics for each variable in the entire subsetted sample, as well as within each stratum if any is defined, are contained in a matrix with rows corresponding to variables and strata and columns corresponding to the descriptive statistics.
%%  If it is a LIST, use
\item{N}{the number of observations.}
\item{Msng}{the number of observations with missing values.}
\item{Mean}{the mean of the nonmissing observations (this is potentially a restricted mean for right censored time to event data).}
\item{Std Dev}{the standard deviation of the nonmissing observations (this is potentially a restricted standard deviation for right censored time to event data).}
\item{Geom Mn}{the geometric mean of the nonmissing observations (this is potentially a restricted geometric mean for right censored time to event data). Nonpositive values in the variable will generate \code{NA}, unless \code{replaceZeroes} was specified.}
\item{Min}{the minimum value of the nonmissing observations  (this is potentially restricted for right censored time to event data).}
\item{-}{columns corresponding to the quantiles specified by \code{probs}}
\item{Max}{the maximum value of the nonmissing observations (this is potentially restricted for right censored time to event data).}
\item{-}{columns corresponding to the proportions as specified by \code{above}, \code{below}, \code{labove}, \code{rbelow}, \code{lbetween}, \code{rbetween}, \code{interval}, \code{linterval}, \code{rinterval}, \code{lrinterval}.}
\item{restriction}{the threshold for restricted means, standard deviations, and geometric means.}
\item{FirstEvent}{the time of the first event for censored time to event variables.}
\item{LastEvent}{the time of the last event for censored time to event variables.}
\item{isDate}{an indicator that the variable is a \code{Date} object.}
}
%%  \item{comp2 }{Description of 'comp2'}
%% ...


\author{
%%  ~~who you are~~
Scott S. Emerson, M.D., Ph.D., Andrew J. Spieker, Brian D. Williamson, Travis Y. Hee Wai, and Solomon Lim}

\examples{
#- Load the data -#
#- Read in and attach the data -#
mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt", header=TRUE)
attach(mri)

#- Load libraries -#
library(survival)

#- Create the table -#
descrip(mri)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
