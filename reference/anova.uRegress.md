# ANOVA

Compute analysis of variance (or deviance) tables for two fitted, nested
`uRegress` objects. The model with more parameters is referred to as the
full model (or the larger model), and the model with fewer parameters is
referred to as the null model (or the smaller model).

## Usage

``` r
# S3 method for class 'uRegress'
anova(object, full_object, test = "LRT", robustSE = TRUE, useFdstn = TRUE, ...)
```

## Arguments

- object:

  an object of class `uRegress`, the model with fewer parameters (i.e.
  the null model).

- full_object:

  an object of class `uRegress`, the model with more parameters (i.e.
  the full model).

- test:

  a character string specifying the test statistic to be used. Can be
  one of `'Wald'` or `'LRT'`, which corresponds to Wald or likelihood
  (partial likelihood for hazard regressions) ratio tests. Note that
  currently the Wald test is only supported for symbolically nested
  models; that is, when the larger model contains all the covariates
  (with the same names) in the smaller model.

- robustSE:

  a logical value indicating whether or not to use robust standard
  errors in calculation. Defaults to `TRUE`. If `TRUE`, then `robustSE`
  must have been `TRUE` when `reg` was created.

- useFdstn:

  a logical indicator that the F distribution should be used for test
  statistics instead of the chi squared distribution. Defaults to
  `FALSE`. This option is not supported when input `reg` is a hazard
  regression (i.e., `fnctl="hazard"`).

- ...:

  argument to be passed in

## Value

A list of class `anova.uRegress` with the following components:

- printMat:

  A formatted table with inferential results (i.e., test statistics and
  p-values) for comparing two nested models.

- null_model:

  The null model in the comparison.

- full_model:

  The full model in the comparison.

## Examples

``` r
# Loading required libraries
library(sandwich)

# Reading in a dataset
data(mri)

# Linear regression of LDL on age and stroke (with robust SE by default)
testReg_null <- regress ("mean", ldl~age+stroke, data = mri)

# Linear regression of LDL on age, stroke, and race (with robust SE by default)
testReg_full <- regress ("mean", ldl~age+stroke+race, data = mri)
# Comparing the two models using the Wald test with robust SE
anova(testReg_null, testReg_full, test = "Wald")
#> Analysis of Deviance Table 
#> Null model: ldl ~ age + stroke
#> <environment: 0x55a6526f9148>
#> 
#> Full model: ldl ~ age + stroke + race
#> <environment: 0x55a6526f9148>
#> 
#>      F stat num df den df p value
#> [1,] 0.1619 3.0000    719   0.922
```
