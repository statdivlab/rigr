# T-test with Improved Layout

Performs a one- or two-sample t-test using data. In the two-sample case,
the user can specify whether or not observations are matched, and
whether or not equal variances should be presumed.

## Usage

``` r
ttest(
  var1,
  var2 = NA,
  by = NA,
  geom = FALSE,
  null.hypoth = 0,
  alternative = "two.sided",
  var.eq = FALSE,
  conf.level = 0.95,
  matched = FALSE,
  more.digits = 0
)
```

## Arguments

- var1:

  a (non-empty) numeric vector of data values.

- var2:

  an optional (non-empty) numeric vector of data.

- by:

  a variable of equal length to that of `var1` with two outcomes. This
  will be used to define strata for a t-test on `var1`.

- geom:

  a logical indicating whether the geometric mean should be calculated
  and displayed.

- null.hypoth:

  a number specifying the null hypothesis for the mean (or difference in
  means if performing a two-sample test). Defaults to zero.

- alternative:

  a string: one of `"less"`, `"two.sided"`, or `"greater"` specifying
  the form of the test. Defaults to a two-sided test.

- var.eq:

  a logical value, either `TRUE` or `FALSE` (default), specifying
  whether or not equal variances should be presumed in a two-sample
  t-test. Also controls robust standard errors.

- conf.level:

  confidence level of the test. Defaults to 0.95.

- matched:

  a logical value, either `TRUE` or `FALSE`, indicating whether or not
  the variables of a two-sample t-test are matched. Variables must be of
  equal length.

- more.digits:

  a numeric value specifying whether or not to display more or fewer
  digits in the output. Non-integers are automatically rounded down.

## Value

a list of class `ttest`. The print method lays out the information in an
easy-to-read format.

- tab:

  A formatted table of descriptive and inferential statistics (total
  number of observations, number of missing observations, mean, standard
  error of the mean estimate, standard deviation), along with a
  confidence interval for the mean.

- df:

  Degrees of freedom for the t-test.

- p:

  P-value for the t-test.

- tstat:

  Test statistic for the t-test.

- var1:

  The user-supplied first data vector.

- var2:

  The user-supplied second data vector.

- by:

  The user-supplied stratification variable.

- par:

  A vector of information about the type of test (null hypothesis,
  alternative hypothesis, etc.)

- geo:

  A formatted table of descriptive and inferential statistics for the
  geometric mean.

- call:

  The call made to the `ttest` function.

## Details

Missing values must be given by `NA` to be recognized as missing values.

## See also

[`t.test`](https://rdrr.io/r/stats/t.test.html)

## Examples

``` r
# Read in data set
data(psa)
attach(psa)
#> The following objects are masked from psa (pos = 3):
#> 
#>     age, bss, grade, inrem, nadirpsa, obstime, pretxpsa, ps, ptid

# Perform t-test
ttest(pretxpsa, null.hypoth = 100, alternative = "greater", more.digits = 1)
#> 
#> Call:
#> ttest(var1 = pretxpsa, null.hypoth = 100, alternative = "greater", 
#>     more.digits = 1)
#> 
#> One-sample t-test :
#>  
#> Summary:
#>  Variable Obs Missing  Mean Std. Err. Std. Dev.        95% CI
#>  pretxpsa  50       7 670.8     196.4      1288 [274.5, 1067]
#> 
#>  Ho:  mean <= 100 ; 
#>  Ha:  mean > 100 
#>  t = 2.9066 , df = 42 
#>  Pr(T > t) =  0.002905293 

# Define new binary variable as indicator
# of whether or not bss was worst possible
bssworst <- bss
bssworst[bss == 1] <- 0
bssworst[bss == 2] <- 0
bssworst[bss == 3] <- 1

# Perform t-test allowing for unequal
# variances between strata -#
ttest(pretxpsa, by = bssworst)
#> 
#> Call:
#> ttest(var1 = pretxpsa, by = bssworst)
#> 
#> Two-sample t-test allowing for unequal variances :
#>  
#> Summary:
#>           Group Obs Missing Mean Std. Err. Std. Dev.          95% CI
#>    bssworst = 0  18       2  119      36.2       145     [41.5, 196]
#>    bssworst = 1  30       4 1016     307.2      1566   [383.5, 1649]
#>      Difference  48       6 -897     309.3      <NA> [-1533.4, -261]
#> 
#>  Ho: difference in  means = 0 ; 
#>  Ha: difference in  means != 0 
#>  t = -2.901 , df = 25.7 
#>  Pr(|T| > t) =  0.00752391 

# Perform matched t-test
ttest(pretxpsa, nadirpsa, matched = TRUE, conf.level = 99/100, more.digits = 1)
#> 
#> Call:
#> ttest(var1 = pretxpsa, var2 = nadirpsa, conf.level = 99/100, 
#>     matched = TRUE, more.digits = 1)
#> 
#> Two-sample (matched) t-test  :
#>  
#> Summary:
#>         Group Obs Missing   Mean Std. Err. Std. Dev.             99% CI
#>      pretxpsa  50       7 670.75    196.36   1287.64 [140.951, 1200.55]
#>      nadirpsa  50       0  16.36      5.55     39.25     [1.486, 31.23]
#>    Difference  50       7 655.88    193.57      1254 [133.622, 1178.14]
#> 
#>  Ho: difference in  means = 0 ; 
#>  Ha: difference in  means != 0 
#>  t = 3.3884 , df = 42 
#>  Pr(|T| > t) =  0.00153846 

```
