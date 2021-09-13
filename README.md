
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start 
[![CRAN status](https://www.r-pkg.org/badges/version/corncob)](https://CRAN.R-project.org/package=corncob)-->

[![R-CMD-check](https://github.com/statdivlab/rigr/workflows/R-CMD-check/badge.svg)](https://github.com/statdivlab/rigr/actions)
[![codecov](https://codecov.io/gh/statdivlab/rigr/branch/main/graph/badge.svg)](https://codecov.io/gh/statdivlab/rigr)
<!-- badges: end -->

# `rigr`: Regression, Inference, and General Data Analysis Tools for R

## Introduction

`rigr` is an `R` package to streamline data analysis in `R`. Learning
both `R` and introductory statistics at the same time can be
challenging, and so we created `rigr` to facilitate common data analysis
tasks and enable learners to focus on statistical concepts.

`rigr`, formerly known as
[`uwIntroStats`](https://CRAN.R-project.org/package=uwIntroStats),
provides easy-to-use interfaces for descriptive statistics, one- and
two-sample inference, and regression analyses. `rigr` output includes
key information while omitting unncessary details that can be confusing
to beginners. Heteroskedasticity-robust (“sandwich”) standard errors are
returned by default, and multiple partial F-tests and tests for
contrasts are easy to specify. A single regression function
(`regress()`) can fit both linear and generalized linear models,
allowing students to more easily make connections between different
classes of models.

## Installation

You can install the development version of `rigr` from CRAN as follows:

    install.packages("rigr")

You can install the development version of `rigr` from GitHub using the
code below.

    remotes::install_github("statdivlab/rigr")

If this produces an error, please run `install.packages("remotes")`
first then try the above line again.

`rigr` is maintained by the
[StatDivLab](http://statisticaldiversitylab.com/), but relies on
community support to log issues and implement new features. Is there a
method you would like to have implemented? Please submit a pull request
or start a
[discussion](https://github.com/statdivlab/rigr/discussions/)!

## Documentation

Examples of how to use the main functions in `rigr` are provided in
three vignettes. One details the `regress` function and its utilities,
one details the `descrip` function for descriptive statistics, and the
third details functions used for one- and two-sample inference,
including `ttest`, `wilcoxon`, and `proptest`.

## Humans

Maintainer: [Amy Willis](http://statisticaldiversitylab.com/)

Authors: [Scott S Emerson](http://www.emersonstatistics.com/), [Brian D
Williamson](https://bdwilliamson.github.io/), [Charles
Wolock](https://cwolock.github.io/), [Taylor
Okonek](https://taylorokonek.github.io/), [Jim
Hughes](https://www.biostat.washington.edu/people/james-hughes), [Amy
Willis](http://statisticaldiversitylab.com/), [Andrew J
Spieker](https://www.vumc.org/biostatistics/person/andrew-spieker-phd/)
and [Travis Y Hee
Wai](https://scholar.google.com/citations?user=WaJn2wIAAAAJ&hl=en/).

## Issues

If you encounter any **bugs**, please [file an
issue](https://github.com/statdivlab/rigr/issues/). Better yet, [submit
a pull request](https://github.com/statdivlab/rigr/pulls/)!

Do you have a **question**? Please first check out the vignettes, then
please post on the
[Discussions](https://github.com/statdivlab/rigr/discussions/).
