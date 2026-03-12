# Wilcoxon Signed Rank and Mann-Whitney-Wilcoxon Rank Sum Test

Performs Wilcoxon signed rank test or Mann-Whitney-Wilcoxon rank sum
test depending on data and logicals entered. Relies heavily on the
function [`wilcox.test`](https://rdrr.io/r/stats/wilcox.test.html). Adds
formatting and variances.

## Usage

``` r
wilcoxon(
  var1,
  var2 = NULL,
  alternative = "two.sided",
  null.hypoth = 0,
  paired = FALSE,
  exact = FALSE,
  correct = FALSE,
  conf.int = FALSE,
  conf.level = 0.95
)
```

## Arguments

- var1:

  numeric vector of data values. Non-finite (missing or infinite) values
  will be omitted.

- var2:

  optional numeric vector of data values. Non-finite (missing or
  infinite) values will be omitted.

- alternative:

  specifies the alternative hypothesis for the test; acceptable values
  are `"two.sided"`, `"greater"`, or `"less"`.

- null.hypoth:

  the value of the null hypothesis.

- paired:

  logical indicating whether the data are paired or not. Default is
  `FALSE`. If `TRUE`, data must be the same length.

- exact:

  logical value indicating whether or not an exact test should be
  computed.

- correct:

  logical indicating whether or not a continuity correction should be
  used and displayed.

- conf.int:

  logical indicating whether or not to calculate and display a
  confidence interval

- conf.level:

  confidence level for the interval. Defaults to 0.95.

## Value

A list of class `wilcoxon` is returned. The print method lays out the
information in an easy-to-read format.

- statistic:

  the value of the test statistic with a name describing it.

- parameter:

  the parameter(s) for the exact distribution of the test statistic.

- p.value:

  the p-value for the test (calculated for the test statistic).

- null.value:

  the parameter `null.hypoth`.

- alternative:

  character string describing the alternative hypothesis.

- method:

  the type of test applied.

- data.name:

  a character string giving the names of the data.

- conf.int:

  a confidence interval for the location parameter (only present if the
  argument `conf.int=TRUE`).

- estimate:

  an estimate of the location parameter (only present if the argument
  `conf.int=TRUE`).

- table:

  a formatted table of rank sum and number of observation values, for
  printing.

- vars:

  a formatted table of variances, for printing.

- hyps:

  a formatted table of the hypotheses, for printing.

- inf:

  a formatted table of inference values, for printing.

## Details

In the one-sample case, the returned confidence interval (when
`conf.int = TRUE`) is a confidence interval for the pseudo-median of the
underlying distribution. In the two-sample case, the function returns a
confidence interval for the median of the difference between samples
from the two distributions. See
[`wilcox.test`](https://rdrr.io/r/stats/wilcox.test.html) for more
information.

## See also

[`wilcox.test`](https://rdrr.io/r/stats/wilcox.test.html)

## Examples

``` r
#- Create the data -#
cf <- c(1153, 1132, 1165, 1460, 1162, 1493, 1358, 1453, 1185, 1824, 1793, 1930, 2075)
healthy <- c(996, 1080, 1182, 1452, 1634, 1619, 1140, 1123, 1113, 1463, 1632, 1614, 1836)

#- Perform the test -#
wilcoxon(cf, healthy, paired=TRUE)
#> 
#>  Wilcoxon signed rank test 
#>          obs sum ranks expected
#> positive  10        71     45.5
#> negative   3        20     45.5
#> zero       0         0      0.0
#> all       13        91     91.0
#>                             
#> unadjusted variance   204.75
#> adjustment for ties     0.00
#> adjustment for zeroes   0.00
#> adjusted variance     204.75
#>                     H0 Ha       
#> Hypothesized Median 0  two.sided
#>   Test Statistic p-value 
#> Z 1.7821         0.074735

#- Perform the test -#
wilcoxon(cf, healthy, conf.int=TRUE)
#> 
#>  Wilcoxon rank sum test 
#>          obs rank sum expected
#> cf        13      194    175.5
#> healthy   13      157    175.5
#> combined  26      351    351.0
#>                           
#> unadjusted variance 380.25
#> adjustment for ties   0.00
#> adjusted variance   380.25
#>                     H0 Ha       
#> Hypothesized Median 0  two.sided
#>   Test Statistic p-value CI          Point Estimate
#> Z 0.94872        0.34276 [-172, 340] 62            
```
