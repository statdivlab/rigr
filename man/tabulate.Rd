\name{tabulate}
\alias{tabulate}
\alias{tabulate.do}
\alias{print.tabulate}
\alias{tabulate.default}
\alias{tabModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Table Variables, with Stratification and Statistical Tests
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Takes in a list of strata, and returns a stratified table of counts (can also include other descriptives as specified by the user) as well as the chi-squared summaries for each. Relies on the \code{Exact} package for calculating Odds Ratios and Risk Ratios, the \code{plyr} package for array manipulation, and the \code{survival} package for censored data analysis.
}
\usage{
## S3 method for class 'tabulate'
tabulate(..., dispRatios=FALSE, stratified=TRUE, tests=NULL, 
        stat="count", na.rm=TRUE, subset=NULL, probs= c(.25,.50,.75), 
        replaceZeroes=FALSE, restriction=Inf, above=NULL, 
        below=NULL, labove=NULL, rbelow=NULL, lbetween=NULL, rbetween=NULL, 
        interval=NULL, linterval=NULL, rinterval=NULL, lrinterval=NULL, 
        version=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
a list of strata variables. All variables must have the same length.
}
\item{dispRatios}{
%%     ~~Describe \code{\dots} here~~
can only be used if the first two variables only take on two values. Default is \code{FALSE}. If \code{TRUE}, displays Odds Ratio and Relative Risk (the user must decide which is appropriate for inference). It is assumed that the lower value of the data is healthy and the higher value is diseased (for example \code{0} might be no stroke and \code{1} would be had stroke).
}
\item{stratified}{
%%     ~~Describe \code{\dots} here~~
default is \code{TRUE}, displays chi-squared statistics by stratum. Also if row/column/total percentages are requested, displays stratified percentages. If \code{FALSE}, only the overall chi-squared statistic is displayed and row/column/total percentages are calculated over all values.
}
\item{tests}{
%%     ~~Describe \code{\dots} here~~
the type of tests to include in addition to the chi-squared test. Options are \code{"lrchisq"} - Likelihood Ratio Chi-squared Test, \code{"lr"} - Likelihood Ratio Test (logistic regression), \code{"fisher"} - Fisher's Exact Test, \code{"mh"} - Mantel-Haenszel Test, \code{"uWald"} - Barnard's Unconditional Exact Test (Wald statistic), \code{"uScore"} - Barnard's Unconditional Exact Test (score statistic), \code{"score"} - calculate Rao's Score statistic (logistic regression), and \code{"wald"} - Wald Chi-square Test (logistic regression). If \code{"mh"} is entered, the first three strata variables must have dimension at least 2.
}
\item{stat, na.rm, subset, probs, replaceZeroes, restriction, above, below, labove, rbelow, lbetween, rbetween, interval, linterval, rinterval, lrinterval, version}{
%%     ~~Describe \code{\dots} here~~
variables passed to \code{descrip()}. For a detailed description, read the file on \code{descrip()}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
If \code{stratified=TRUE}, prints the stratified count tables and chi-squared summaries. The printed result is a matrix with columns for (if appropriate): the point estimate, test statistic, degrees of freedom, 95\% confidence interval, p-value, and any warnings in computation. If requested, row/column/total percentages are also stratified based on the first two variables entered. If \code{stratified=FALSE}, then the overall chi-squared statistic is returned and percentages are calculated from the overall table. If specified the user can also display Odds Ratio/Risk Ratio, likelihood-ratio test, Fisher's Exact test, Wald test, Barnard's Unconditional Exact test, Rao's Score test, and Mantel-Haenszel test results. The Mantel-Haenzsel chi-squared estimate, confidence interval, and estimate of the odds ratio are only returned if the first three variables entered into \code{tabulate()} are 2 by 2 by K. Any call to \code{tabulate()} will run \code{tabulate.default()}, with user specified values in place of the appropriate defaults.
}
\value{
Returns an object of class \code{tabulate}. Contains two values in a list:
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{rslt}{the raw result, which has all of the tables of counts.}
\item{printer}{the raw result with descriptive statistics, if specified.}
%% ...
}
\author{
%%  ~~who you are~~
Scott S. Emerson, M.D., Ph.D., Andrew J. Spieker, Brian D. Williamson, Travis Y. Hee Wai, and Solomon Lim
}

\seealso{
\code{\link[Exact]{exact.test}}, \code{\link[stats]{fisher.test}}, \code{\link[plyr]{aaply}}, \code{\link[uwIntroStats]{descrip}}, \code{\link[uwIntroStats]{tableStat}}
}
\examples{
## load the necessary libraries
library(survival)
library(Exact)
library(plyr)

## read in the mri dataset 
mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt",header=TRUE)

## attach the mri dataset
attach(mri)

## create a table of stroke and race
tabulate(stroke, race)

## perform a chi-squared test of stroke vs race, display the count, row percentage, 
## and column percentage
tabulate(stroke, race, stat="@count@ @row\%@ @col\%@")

## perform chi-squared test, likelihood ratio test, and fisher's exact test 
## for stroke vs race
tabulate(stroke, race, tests=c("lrchisq", "fisher"))

## for diabetes vs male by race, perform chi-squared test, display 
## odds ratio/risk ratio, mantel-haenzsel, likelihood ratio chi-squared
tabulate(diabetes, male, race, dispRatios=TRUE, tests=c("lrchisq", "mh"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
