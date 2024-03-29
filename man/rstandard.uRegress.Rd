% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regress_diagnostics.R
\name{rstandard.uRegress}
\alias{rstandard.uRegress}
\title{Extract standardized residuals from \code{uRegress} objects}
\usage{
\method{rstandard}{uRegress}(model, ...)
}
\arguments{
\item{model}{an object of class \code{uRegress}, as returned by
\link[rigr]{regress}.}

\item{...}{other arguments to pass to \code{residuals.uRegress}}
}
\value{
a vector of standardized residuals
}
\description{
Extracts standardized residuals from \code{uRegress} objects by relying on
functionality from the \code{stats} package.
}
