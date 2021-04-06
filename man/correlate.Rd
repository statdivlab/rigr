\name{correlate}
\alias{correlate}
\alias{print.uCorrelate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Correlation 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Computes correlation matrix for an arbitrary number of numeric variables, optionally within strata.
}
\usage{
correlate(..., strata = NULL, subset = NULL, conf.level = 0.95, 
          use = "pairwise.complete.obs", method = "pearson", 
          stat = "cor", byStratum = TRUE, version = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
an arbitrary number of variables for which a correlation matrix is desired. The
arguments can be vectors, matrices, or lists. Individual columns of a matrix or
elements of a list that are not of class \code{numeric}, \code{factor}, or \code{Date} will be omitted.
Factor and Date variables are converted to integers. Character vectors will be
coerced to numeric. Variables must all be of the same lengths.
}
  \item{strata}{
%%     ~~Describe \code{strata} here~~
vector, matrix, or list of stratification variables. Descriptive statistics will be
computed within strata defined by each unique combination of the stratification
variables, as well as in the combined sample. If \code{strata} is supplied, all variables
must be of that same length.

}
  \item{subset}{
%%     ~~Describe \code{subset} here~~
vector indicating a subset to be used for all descriptive statistics. If \code{subset} is
supplied, all variables must be of that same length.
}
  \item{conf.level}{
%%     ~~Describe \code{conf.level} here~~
a numeric scalar between 0 and 1 denoting the confidence level to be used in
constructing confidence intervals for the correlation.

}
  \item{use}{
%%     ~~Describe \code{use} here~~
character string denoting the cases to use: \code{"everything"} uses all cases (and causes
NA when any needed variable is missing), \code{"complete.obs"} uses only those rows
with no missing data for any variable, and \code{"pairwise.complete.obs"} computes
pairwise correlations using all cases that are not missing data for the relevant
variables.
}
  \item{method}{
%%     ~~Describe \code{method} here~~
character string denoting the correlation method to use: \code{"pearson"} denotes
Pearson's correlation coefficient and \code{"spearman"} denotes Spearman's rank
correlation.
}
  \item{stat}{
%%     ~~Describe \code{stat} here~~
a vector of character strings indicating the descriptive statistic(s) to be tabulated.
Possibilities include any statistic as specified by one or more of \code{"cor"}, \code{"n"},
\code{"t.stat"}, \code{"pval"}, \code{"loCI"}, or \code{"hiCI"}. Only enough of the string needs to be
specified to disambiguate the choice. Alternatively (and more usefully), a single
special format character string can be specified as described in the Details below.
}
  \item{byStratum}{
%%     ~~Describe \code{byStratum} here~~
a logical scalar indicating whether statistics should be grouped by pair of
variables. If \code{TRUE}, the results will be displayed in a series of tables where each
table correspond to a single variable, with rows corresponding to different strata
and columns reflecting all other variables. If \code{FALSE}, the results will be displayed
in a series of tables where each table corresponds to a single stratum and rows
and columns reflect the variables.

}
  \item{version}{
%%     ~~Describe \code{version} here~~
if \code{TRUE}, the version of the function will be returned. No other computations will
be performed.
}
}

\value{
%%  ~Describe the value returned
An object of class uCorrelate is returned, which consists of a list of correlation estimates and inference
for each specified stratum and for the combined dataset. Each element of the list has six arrays: 
  \item{cormtx}{the correlation matrix, printed.}
  \item{n}{matrix of sample sizes used to compute each correlation}
  \item{t.stat}{matrix of t-statistics, testing a correlation of 0.}
  \item{pval}{matrix of two-sided p-values for the t-test.}
  \item{lo95\%CI}{lower bound of the 95\% confidence interval.}
  \item{hi95\%CI}{upper bound of the 95\% confidence interval.}

%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
%%  ~~who you are~~
Scott S. Emerson, M.D., Ph.D., Andrew J. Spieker, Brian D. Williamson, Travis Y. Hee Wai, and Solomon Lim}

\examples{
# Load required libraries
library(survival)

# Reading in a dataset
mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt",header=TRUE)

# Estimated correlation matrix using all data, complete cases, or pairwise complete (the default)
with (mri, correlate(age,weight,ldl,use="everything"))
with (mri, correlate(age,weight,ldl,use="complete"))
with (mri, correlate(age,weight,ldl))

# Correlation matrices for each stratum
with (mri, correlate(age,weight,ldl,strata=male))

# Correlations grouped by variable
with (mri, correlate(age,weight,ldl,strata=male,byStratum=FALSE))

# Special formatting of inference for correlations within strata
with (mri, correlate(age,weight,ldl,strata=male,stat="@cor@ (@lo@, @hi@); P @p@; n= @n@"))

# Special formatting of inference for correlations grouped by variable
with (mri, correlate(age,weight,ldl,strata=male,stat="@cor@ (@lo@, @hi@); P @p@; n= @n@",
      byStratum=FALSE))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
