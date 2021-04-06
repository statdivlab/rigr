\name{oneSample}
\alias{oneSample}
\alias{binomInference.exactLR}
\alias{binomInference.exactTail}
\alias{binomInference.halfP}
\alias{binomInference.jeffreys}
\alias{binomInference.wald}
\alias{binomInference.cwald}
\alias{binomInference.score}
\alias{binomInference.cscore}
\alias{binomInference.agresti}
\alias{KMinference.ident}
\alias{CIptKM}
\alias{CIefrKM}
\alias{CIhwKM}
\alias{qSupBrnMotn}
\alias{qSupBrnBrdg}
\alias{print.uOneSample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
One Sample Inferential Methods
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Produces point estimates, interval estimates, and p values for an arbitrary functional (mean, geometric mean,
proportion, median, quantile, odds) of a variable of class \code{integer}, \code{numeric}, \code{Surv}, or \code{Date}. A variety of
inferential methods are provided, with the choices depending on the functional and the data type.

}
\usage{
oneSample(fnctl, y, null.hypothesis = NA, test.type = "two.sided", subset = rep(TRUE, N), 
          conf.level = 0.95, na.rm = TRUE, probs = 0.5, replaceZeroes = NULL, 
          restriction = Inf, subjTime = rep(1, N), method = NULL, above = NULL, 
          below = NULL, labove = NULL, rbelow = NULL, interval = NULL, linterval = NULL, 
          rinterval = NULL, lrinterval = NULL, g1 = 1, g2 = 0, dispersion = 1, 
          nbstrap = 10000, resample = "pairs", seed = 42, ..., version = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fnctl}{
%%     ~~Describe \code{fnctl} here~~
a character string indicating the functional (summary measure of the distribution) for which inference is desired. Choices include \code{"mean"}, \code{"geometric mean"}, \code{"proportion"}, \code{"median"}, \code{"quantile"}, \code{"odds"}, \code{"rate"}. The character string may be shortened to a unique substring. Hence \code{"mea"} will suffice for \code{"mean"}.
}
  \item{y}{
%%     ~~Describe \code{y} here~~
a variable for which inference is desired. The variable may be of class \code{numeric}, \code{Surv}, or \code{Date}.
}
  \item{null.hypothesis}{
%%     ~~Describe \code{null.hypothesis} here~~
a numeric scalar indicating any null hypothesis to be tested.
}
  \item{test.type}{
%%     ~~Describe \code{test.type} here~~
a character string indicating whether a hypothesis test is to be of a one sided test of a lesser alternative hypothesis (\code{"less"}), a one sided test of a greater alternative hypothesis (\code{"greater"}), or a test of a two sided alternative hypothesis (\code{"two.sided"}). The default value is \code{"two.sided"}.
}
  \item{subset}{
%%     ~~Describe \code{subset} here~~
a vector indicating a subset to be used for all inference.
}
  \item{conf.level}{
%%     ~~Describe \code{conf.level} here~~
a numeric scalar indicating the level of confidence to be used in computing confidence intervals. The default is 0.95.
}
  \item{na.rm}{
%%     ~~Describe \code{na.rm} here~~
an indicator that missing data is to be removed prior to computation of the
descriptive statistics.
}
  \item{probs}{
%%     ~~Describe \code{probs} here~~
a vector of probabilities between 0 and 1 indicating quantile estimates to be included in the descriptive statistics. Default is to the 50th (median) percentile.
}
  \item{replaceZeroes}{
%%     ~~Describe \code{replaceZeroes} here~~
if not \code{FALSE}, this indicates a value to be used in place of zeroes when computing a geometric mean. If \code{TRUE}, a value equal to one-half the lowest nonzero value is used. If a numeric value is supplied, that value is used for all variables.
}
  \item{restriction}{
%%     ~~Describe \code{restriction} here~~
a value used for computing restricted means, standard deviations, and geometric means with censored time to event data. The default value of \code{Inf} will cause restrictions at the highest observation. Note that the same value is used for all variables of class \code{Surv}.
}
  \item{subjTime}{
%%     ~~Describe \code{subjTime} here~~
a vector of values for use with rates.
}
  \item{method}{
%%     ~~Describe \code{method} here~~
a character string used to indicate inferential methods. Allowed choices depend on the variable type and the functional. Default values are \code{"t.test"} for means and geometric means, and \code{"exact"} for proportions of uncensored data, and \code{"KM"} for censored survival data.
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
  \item{g1}{
%%     ~~Describe \code{g1} here~~
used in \code{method="mean-variance"}.
}
  \item{g2}{
%%     ~~Describe \code{g2} here~~
used in \code{method="mean-variance"}.

}
  \item{dispersion}{
%%     ~~Describe \code{dispersion} here~~
dispersion, used in \code{method="mean-variance"}.
}
  \item{nbstrap}{
%%     ~~Describe \code{nbstrap} here~~
number of bootstrap iterations to perform, used with \code{method="bootstrap"}.
}
  \item{resample}{
%%     ~~Describe \code{resample} here~~
character string specifying how the bootstrap should resample, used with \code{method="bootstrap"}.

}
  \item{seed}{
%%     ~~Describe \code{seed} here~~
sets the seed (for random number generation), used with \code{method="bootstrap"}.

}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
other arguments.
}
  \item{version}{
%%     ~~Describe \code{version} here~~
if \code{TRUE}, the version of the function will be returned. No other computations will
be performed.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Default values for inference correspond to the most commonly implemented methods. Additional methods
are provided more for educational purposed than for purposes of statistical analysis.

}
\value{
%%  ~Describe the value returned
An object of class \code{uOneSample} is returned. Inferential statistics are contained in a vector named \code{$Inference} that includes the sample size, the point estimate, the lower and upper bounds of a confidence interval, any null hypothesis that was specified, and the p-value. Also included is a vector named \code{$Statistics} that includes more technical information. There is a print method that will format the descriptive statistics for the \code{Date} and \code{Surv} objects.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
%%  ~~who you are~~
Scott S. Emerson, M.D., Ph.D., Andrew J. Spieker, Brian D. Williamson, Travis Y. Hee Wai, Solomon Lim
}
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

# Inference about the mean LDL: a two sample t test that mean LDL is 135 mg/dl
oneSample ("mean", mri$ldl, null.hypothesis=125)

# Inference about the mean LDL: a one sample t test of a lesser alternative
# that mean LDL is 125 mg/dl
oneSample ("mean", mri$ldl, null.hypothesis=125, test.type="less")

# Inference about the mean LDL: a one sample t test of a greater alternative
# that mean LDL is 125 mg/dl
oneSample ("mean", mri$ldl, null.hypothesis=125, test.type="greater")

# Inference about the geometric mean LDL: a one sample t test of a greater
# alternative that geometric mean LDL is 125 mg/dl
oneSample ("geom", mri$ldl, null.hypothesis=125, test.type="greater")

# Inference about the proportion of subjects with LDL greater than 128: exact binomial
# inference that 50\% of subjects have LDL greater than 128 mg/dl
oneSample ("prop", mri$ldl, null.hypothesis=0.5, above=128)
oneSample ("prop",mri$ldl>128, null.hypothesis=0.5)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
