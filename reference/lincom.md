# Tests of Linear Combinations of Regression Coefficients

Produces point estimates, interval estimates, and p-values for linear
combinations of regression coefficients using a `uRegress` object.

## Usage

``` r
lincom(
  reg,
  comb,
  null.hypoth = 0,
  conf.level = 0.95,
  robustSE = TRUE,
  joint.test = FALSE,
  useFdstn = FALSE,
  eform = reg$fnctl != "mean"
)
```

## Arguments

- reg:

  an object of class `uRegress`.

- comb:

  a vector or matrix containing the values of the constants which create
  the linear combination of the form \$\$c_0 + c_1\beta_1 + \dots\$\$
  Zeroes must be given if coefficients aren't going to be included. For
  testing multiple combinations, this must be a matrix with number of
  columns equal to the number of coefficients in the model.

- null.hypoth:

  the null hypothesis to compare the linear combination of coefficients
  against. This is a scalar if one combination is given, and a vector or
  matrix otherwise. The default value is `0`.

- conf.level:

  a number between 0 and 1, indicating the desired confidence level for
  intervals.

- robustSE:

  a logical value indicating whether or not to use robust standard
  errors in calculation. Defaults to `TRUE`. If `TRUE`, then `robustSE`
  must have been `TRUE` when `reg` was created.

- joint.test:

  a logical value indicating whether or not to use a joint Chi-square
  test for all the null hypotheses. If joint.test is `TRUE`, then no
  confidence interval is calculated. Defaults to `FALSE`.

- useFdstn:

  a logical indicator that the F distribution should be used for test
  statistics instead of the chi squared distribution. Defaults to
  `TRUE`. This option is not supported when input `reg` is a hazard
  regression (i.e., `fnctl="hazard"`).

- eform:

  a logical value indicating whether or not to exponentiate the
  estimated coefficient. By default this is performed based on the type
  of regression used.

## Value

A list of class `lincom` (`joint.test` is `False`) or `lincom.joint`
(`joint.test` is `True`). For the `lincom` class, `comb` entries in the
list are labeled `comb1`, `comb2`, etc. for as many linear combinations
were used. Each is a list with the following components:

- printMat:

  A formatted table with inferential results for the linear combination
  of coefficients. These include the point estimate, standard error,
  confidence interval, and t-test for the linear combination.

- nms:

  The name of the linear combination, for printing.

- null.hypoth:

  The null hypothesis for the linear combination.

## Examples

``` r
# Loading required libraries
library(sandwich)

# Reading in a dataset
data(mri)

# Linear regression of LDL on age (with robust SE by default)
testReg <- regress ("mean", ldl~age+stroke, data = mri)

# Testing coefficient created by .5*age - stroke (the first 0 comes from excluding the intercept)
testC <- c(0, 0.5, -1)
lincom(testReg, testC)
#> 
#> H0: 0.5*age-1*stroke   =  0 
#> Ha: 0.5*age-1*stroke  !=  0 
#>      Estimate Std. Err.   95%L   95%H      T Pr(T > |t|)
#> [1,]   -1.367     2.152 -5.593  2.859 -0.635       0.526

# Test multiple combinations: 
# whether separately whether .5*age - stroke = 0 or Intercept + 60*age = 125 
testC <- matrix(c(0, 0.5, -1, 1, 60, 0), byrow = TRUE, nrow = 2)
lincom(testReg, testC, null.hypoth = c(0, 125))
#> 
#> H0: 0.5*age-1*stroke   =  0 
#> Ha: 0.5*age-1*stroke  !=  0 
#>      Estimate Std. Err.   95%L   95%H      T Pr(T > |t|)
#> [1,]   -1.367     2.152 -5.593  2.859 -0.635       0.526
#> 
#> H0: 1*(Intercept)+60*age   =  125 
#> Ha: 1*(Intercept)+60*age  !=  125 
#>      Estimate Std. Err.    95%L    95%H     T Pr(T > |t|)
#> [1,]  126.989     3.568 119.984 133.994 0.557       0.577

# Test joint null hypothesis:
# H0: .5*age - stroke = 0 AND Intercept + 60*age = 125 
lincom(testReg, testC, null.hypoth = c(0, 125), joint.test = TRUE)
#>      Chi2 stat df p value
#> [1,]    0.6911  2   0.708
```
