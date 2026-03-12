# Test of proportions from summary statistics

Performs a one- or two-sample test of proportions using counts of
successes and trials, rather than binary data. This test can be
approximate or exact.

## Usage

``` r
proptesti(
  x1,
  n1,
  x2 = NULL,
  n2 = NULL,
  exact = FALSE,
  null.hypoth = ifelse(is.null(x2) && is.null(n2), 0.5, 0),
  conf.level = 0.95,
  alternative = "two.sided",
  correct = FALSE,
  more.digits = 0
)
```

## Arguments

- x1:

  Number of successes in first sample

- n1:

  Number of trials in first sample

- x2:

  Number of successes in second sample

- n2:

  Number of trials in second sample

- exact:

  If true, performs a test of equality of proportions with Exact
  Binomial based confidence intervals.

- null.hypoth:

  a number specifying the null hypothesis for the mean (or difference in
  means if performing a two-sample test). Defaults to 0.5 for one-sample
  and 0 for two-sample.

- conf.level:

  confidence level of the test. Defaults to 0.95

- alternative:

  a string: one of `"less"`, `"two.sided"`, or `"greater"` specifying
  the form of the test. Defaults to a two-sided test. When either
  `"less"` or `"greater"` is used, the corresponding one-sided
  confidence interval is returned.

- correct:

  a logical indicating whether to perform a continuity correction

- more.digits:

  a numeric value specifying whether or not to display more or fewer
  digits in the output. Non-integers are automatically rounded down.

## Value

A list of class `proptesti`. The print method lays out the information
in an easy-to-read format.

- tab:

  A formatted table of descriptive and inferential results (total number
  of observations, sample proportion, standard error of the proportion
  estimate), along with a confidence interval for the underlying
  proportion.

- zstat:

  the value of the test statistic, if using an approximate test.

- pval:

  the p-value for the test

- par:

  A vector of information about the type of test (null hypothesis,
  alternative hypothesis, etc.)

## Details

If `x2` or `n2` are specified, then both must be specified, and a
two-sample test is run.

## Examples

``` r
# Two-sample test
proptesti(10, 100, 15, 200, alternative = "less")
#> 
#> Call:
#> proptesti(x1 = 10, n1 = 100, x2 = 15, n2 = 200, alternative = "less")
#> 
#> Two-sample proportion test (approximate)  :
#>  
#>         Group Obs  Mean Std. Err.            95% CI
#>          var1 100   0.1      0.03  [0.0412, 0.1588]
#>          var2 200 0.075    0.0186  [0.0385, 0.1115]
#>    Difference 300 0.025    0.0353 [-0.0442, 0.0942]
#> Summary:
#> 
#>  Ho: Difference in proportions >= 0 
#>  Ha: Difference in proportions < 0 
#>  Z = 0.739 
#>  p.value = 0.77 
```
