# Test of proportions with improved layout

Performs a one- or two-sample test of proportions using data. This test
can be approximate or exact.

## Usage

``` r
proptest(
  var1,
  var2 = NULL,
  by = NULL,
  exact = FALSE,
  null.hypoth = ifelse(is.null(var2) && is.null(by), 0.5, 0),
  alternative = "two.sided",
  conf.level = 0.95,
  correct = FALSE,
  more.digits = 0
)
```

## Arguments

- var1:

  a (non-empty) vector of binary numeric (0-1), binary factor, or
  logical data values

- var2:

  an optional (non-empty) vector of binary numeric (0-1), binary factor,
  or logical data values

- by:

  a variable of equal length to that of `var1` with two outcomes
  (numeric or factor). This will be used to define strata for a prop
  test on `var1`.

- exact:

  If true, performs a test of equality of proportions using exact
  binomial probabilities.

- null.hypoth:

  a number specifying the null hypothesis for the mean (or difference in
  means if performing a two-sample test). Defaults to 0.5 for a
  one-sample test and 0 for a two-sample test.

- alternative:

  a string: one of `"less"`, `"two.sided"`, or `"greater"` specifying
  the form of the test. Defaults to a two-sided test.

- conf.level:

  confidence level of the test. Defaults to 0.95.

- correct:

  a logical indicating whether to perform a continuity correction

- more.digits:

  a numeric value specifying whether or not to display more or fewer
  digits in the output. Non-integers are automatically rounded down.

## Value

A list of class `proptest`. The print method lays out the information in
an easy-to-read format.

- tab:

  A formatted table of descriptive and inferential results (total number
  of observations, number of missing observations, sample proportion,
  standard error of the proportion estimate), along with a confidence
  interval for the underlying proportion.

- zstat:

  the value of the test statistic, if using an approximate test.

- pval:

  the p-value for the test

- var1:

  The user-supplied first data vector.

- var2:

  The user-supplied second data vector.

- by:

  The user-supplied stratification variable.

- par:

  A vector of information about the type of test (null hypothesis,
  alternative hypothesis, etc.)

## Details

Missing values must be given by `"NA"`s to be recognized as missing
values. Numeric data must be given in 0-1 form. This function also
accepts binary factor variables, treating the higher level as 1 and the
lower level as 0, or logical variables.

## See also

[`prop.test`](https://rdrr.io/r/stats/prop.test.html)

## Examples

``` r
# Read in data set
data(psa)
attach(psa)

# Define new binary variable as indicator
# of whether or not bss was worst possible
bssworst <- bss
bssworst[bss == 1] <- 0
bssworst[bss == 2] <- 0
bssworst[bss == 3] <- 1


# Perform test comparing proportion in remission
# between bss strata
proptest(factor(inrem), by = bssworst)
#> 
#> Call:
#> proptest(var1 = factor(inrem), by = bssworst)
#> 
#> Two-sample proportion test (approximate) :
#>  
#>           Group Obs Missing      Mean Std. Err.         95% CI
#>    bssworst = 0  18       0       0.5     0.118 [0.269, 0.731]
#>    bssworst = 1  30       0 0.1666667     0.068  [0.0333, 0.3]
#>      Difference  48       0 0.3333333     0.136  [0.0666, 0.6]
#> Summary:
#> 
#>  Ho: Difference in proportions = 0 
#>  Ha: Difference in proportions != 0 
#>  Z = 2.46 
#>  p.value = 0.0139 
```
