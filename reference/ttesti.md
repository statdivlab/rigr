# T-test Given Summary Statistics with Improved Layout

Performs a one- or two-sample t-test given summary statistics. In the
two-sample case, the user can specify whether or not equal variances
should be presumed.

## Usage

``` r
ttesti(
  obs,
  mean,
  sd,
  obs2 = NA,
  mean2 = NA,
  sd2 = NA,
  null.hypoth = 0,
  conf.level = 0.95,
  alternative = "two.sided",
  var.eq = FALSE,
  more.digits = 0
)
```

## Arguments

- obs:

  number of observations for the first sample.

- mean:

  the sample mean of the first sample.

- sd:

  the sample standard deviation of the first sample.

- obs2:

  number of observations for the second sample (this is optional).

- mean2:

  if `obs2` is supplied, then sample mean of the second sample must be
  supplied.

- sd2:

  if `obs2` is supplied, then sample standard deviation of the second
  sample must be supplied.

- null.hypoth:

  a number specifying the null hypothesis for the mean (or difference in
  means if performing a two-sample test). Defaults to zero.

- conf.level:

  confidence level of the test. Defaults to 0.95.

- alternative:

  a string: one of `"less"`, `"two.sided"`, or `"greater"` specifying
  the form of the test. Defaults to a two-sided test.

- var.eq:

  a logical value, either `TRUE` or `FALSE` (default), specifying
  whether or not equal variances should be presumed in a two-sample
  t-test.

- more.digits:

  a numeric value specifying whether or not to display more or fewer
  digits in the output. Non-integers are automatically rounded down.

## Value

a list of class `ttesti`. The print method lays out the information in
an easy-to-read format.

- tab:

  A formatted table of descriptive and inferential statistics (number of
  observations, mean, standard error of the mean estimate, standard
  deviation), along with a confidence interval for the mean.

- df:

  Degrees of freedom for the t-test.

- p:

  P-value for the t-test.

- tstat:

  Test statistic for the t-test.

- par:

  A vector of information about the type of test (null hypothesis,
  alternative hypothesis, etc.)

- twosamp:

  A logical value indicating whether a two-sample test was performed.

- call:

  The call made to the `ttesti` function.

## Details

If `obs2`, `mean2`, or `sd2` is specified, then all three must be
specified and a two-sample t-test is run.

## Examples

``` r
# t-test given sample descriptives
ttesti(24, 175, 35, null.hypoth=230)
#> 
#> Call:
#> ttesti(obs = 24, mean = 175, sd = 35, null.hypoth = 230)
#> 
#> One-sample t-test :
#>  
#> Summary:
#>      Obs Mean Std. Error Std. Dev. 95% CI    
#> var1 24  175  7.14       35        [160, 190]
#> 
#>  Ho:  mean = 230 ; 
#>  Ha:  mean != 230 
#>  t = -7.698 , df = 23 
#>  Pr(|T| > t) =  8.23997e-08 

# two-sample test
ttesti(10, -1.6, 1.5, 30, -.7, 2.1)
#> 
#> Call:
#> ttesti(obs = 10, mean = -1.6, sd = 1.5, obs2 = 30, mean2 = -0.7, 
#>     sd2 = 2.1)
#> 
#> Two-sample t-test allowing for unequal variances :
#>  
#> Summary:
#>      Obs Mean Std. Error Std. Dev. 95% CI         
#> var1 10  -1.6 0.474      1.5       [-2.67, -0.527]
#> var2 30  -0.7 0.383      2.1       [-1.48, 0.0842]
#> diff 40  -0.9 0.61       <NA>      [-2.13, 0.335] 
#> 
#>  Ho: difference in  means = 0 ; 
#>  Ha: difference in  means != 0 
#>  t = -1.476 , df = 21.7239 
#>  Pr(|T| > t) =  0.154397 
```
