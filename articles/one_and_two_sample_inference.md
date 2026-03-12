# One- and two-sample inference in rigr

``` r
library(rigr)
```

    ## rigr version 1.0.8: Regression, Inference, and General Data Analysis Tools in R

The `rigr` package replicates many of the basic inferential functions
from R’s `stats` package, with an eye toward inference as taught in an
introductory statistics class. To demonstrate these basic functions, we
will use the included `mri` dataset. Information about the dataset can
be found by running
[`?mri`](https://statdivlab.github.io/rigr/reference/mri.md). Since the
data is part of the package, we can load it via

``` r
data(mri)
```

Throughout this vignette, we will assume familiarity with basic data
manipulation and statistical tasks.

## One and two-sample inference

Many of our analyses boil down to one-sample or two-sample problems,
such as “What is the mean time to graduation?”, “What is the median home
price in Seattle?”, or “What is the difference in mean time to a relapse
event between the control and the treatment group?” There are many
methods of analyzing one- and two-sample relationships, and in our
package we have implemented three common approaches.

### t-tests

We are often interested in making statements about the average (or
*mean*) value of a variable. A one-sample t-test asks whether the mean
of the distribution from which a sample is drawn is equal to some fixed
value. A two-sample t-test asks whether the difference in means between
two distributions is equal to some value (often zero, i.e., no
difference in means).

Our function
[`ttest()`](https://statdivlab.github.io/rigr/reference/ttest.md) is
flexible, allowing stratification, calculation of the geometric mean,
and equal/unequal variances between samples. For example, a t-test of
whether the mean of the `ldl` variable is equal to 125 mg/dL can be
performed using `rigr` as follows:

``` r
ttest(mri$ldl, null.hypoth = 125)
```

    ## 
    ## Call:
    ## ttest(var1 = mri$ldl, null.hypoth = 125)
    ## 
    ## One-sample t-test :
    ##  
    ## Summary:
    ##  Variable Obs Missing Mean Std. Err. Std. Dev.     95% CI
    ##   mri$ldl 735      10  126      1.25      33.6 [123, 128]
    ## 
    ##  Ho:  mean = 125 ; 
    ##  Ha:  mean != 125 
    ##  t = 0.6433 , df = 724 
    ##  Pr(|T| > t) =  0.520256

Note that in addition to running the hypothesis test, `ttest` also
returns a point estimate (the column `Mean` under `Summary`) and a 95%
confidence interval for the true mean LDL.

If instead we wanted a two-sample t-test of whether the difference in
mean LDL between males and females were zero, we could stratify using
the `by` argument:

``` r
ttest(mri$ldl, by = mri$sex)
```

    ## 
    ## Call:
    ## ttest(var1 = mri$ldl, by = mri$sex)
    ## 
    ## Two-sample t-test allowing for unequal variances :
    ##  
    ## Summary:
    ##               Group Obs Missing  Mean Std. Err. Std. Dev.         95% CI
    ##    mri$sex = Female 369       4 130.9      1.79      34.3 [127.4, 134.5]
    ##      mri$sex = Male 366       6 120.6      1.69      32.1 [117.3, 123.9]
    ##          Difference 735      10  10.3      2.47      <NA>    [5.5, 15.2]
    ## 
    ##  Ho: difference in  means = 0 ; 
    ##  Ha: difference in  means != 0 
    ##  t = 4.194 , df = 721 
    ##  Pr(|T| > t) =  3.08428e-05

In addition to using `by`, we can also run two-sample tests by simply
providing two data vectors:
`ttest(mri$ldl[mri$sex == "Female"], mri$ldl[mri$sex == "Male"])`.

Note that the default of `ttest` is to assume unequal variances between
groups, which we (the authors of this package) believe to be the best
choice in most scenarios. We also run two-sided tests by default, but
others can be specified, along with non-zero null hypotheses, and tests
at levels other than 0.95:

``` r
ttest(mri$ldl, null.hypoth = 125, conf.level = 0.9)
```

    ## 
    ## Call:
    ## ttest(var1 = mri$ldl, null.hypoth = 125, conf.level = 0.9)
    ## 
    ## One-sample t-test :
    ##  
    ## Summary:
    ##  Variable Obs Missing Mean Std. Err. Std. Dev.     90% CI
    ##   mri$ldl 735      10  126      1.25      33.6 [124, 128]
    ## 
    ##  Ho:  mean = 125 ; 
    ##  Ha:  mean != 125 
    ##  t = 0.6433 , df = 724 
    ##  Pr(|T| > t) =  0.520256

``` r
ttest(mri$ldl, by = mri$sex, var.eq = FALSE)
```

    ## 
    ## Call:
    ## ttest(var1 = mri$ldl, by = mri$sex, var.eq = FALSE)
    ## 
    ## Two-sample t-test allowing for unequal variances :
    ##  
    ## Summary:
    ##               Group Obs Missing  Mean Std. Err. Std. Dev.         95% CI
    ##    mri$sex = Female 369       4 130.9      1.79      34.3 [127.4, 134.5]
    ##      mri$sex = Male 366       6 120.6      1.69      32.1 [117.3, 123.9]
    ##          Difference 735      10  10.3      2.47      <NA>    [5.5, 15.2]
    ## 
    ##  Ho: difference in  means = 0 ; 
    ##  Ha: difference in  means != 0 
    ##  t = 4.194 , df = 721 
    ##  Pr(|T| > t) =  3.08428e-05

If we prefer to run the test using summary statistics (sample mean,
sample standard deviation, and sample size) we can instead use the
`ttesti` function:

``` r
ttesti(length(mri$weight), mean(mri$weight), sd(mri$weight), null.hypoth = 155)
```

    ## 
    ## Call:
    ## ttesti(obs = length(mri$weight), mean = mean(mri$weight), sd = sd(mri$weight), 
    ##     null.hypoth = 155)
    ## 
    ## One-sample t-test :
    ##  
    ## Summary:
    ##      Obs Mean Std. Error Std. Dev. 95% CI    
    ## var1 735 160  1.13       30.7      [158, 162]
    ## 
    ##  Ho:  mean = 155 ; 
    ##  Ha:  mean != 155 
    ##  t = 4.365 , df = 734 
    ##  Pr(|T| > t) =  1.45125e-05

The result is the same as that provided by
`ttest(mri$weight, null.hypoth = 155)`.

### Tests of proportions

In the above example, we investigated the mean of a continuous random
variable. However, sometimes we work with binary data. In this case, we
may wish to make inference on probabilities. In `rigr`, we can do this
using `proptest`. For one-sample proportion tests, there are both
approximate (based on the normal distribution) and exact (based on the
binomial distribution) options. For example, we may wish to test whether
the proportion of LDL values that are greater than 128mg/dL is equal to
0.5.

``` r
proptest(mri$ldl > 128, null.hypoth = 0.5, exact = FALSE)
```

    ## 
    ## Call:
    ## proptest(var1 = mri$ldl > 128, exact = FALSE, null.hypoth = 0.5)
    ## 
    ## One-sample proportion test (approximate) :
    ##  
    ##       Variable Obs Missing  Estimate Std. Err.       95% CI
    ##  mri$ldl > 128 735      10 0.4634483    0.0185 [0.427, 0.5]
    ## Summary:
    ## 
    ##  Ho: True proportion is = 0.5; 
    ##  Ha: True proportion is != 0.5 
    ##  Z = -1.97 
    ##  p-value = 0.049

``` r
proptest(mri$ldl > 128, null.hypoth = 0.5, exact = TRUE)
```

    ## 
    ## Call:
    ## proptest(var1 = mri$ldl > 128, exact = TRUE, null.hypoth = 0.5)
    ## 
    ## One-sample proportion test (exact) :
    ##  
    ##       Variable Obs Missing  Estimate Std. Err.         95% CI
    ##  mri$ldl > 128 735      10 0.4634483    0.0185 [0.427, 0.501]
    ## Summary:
    ## 
    ##  Ho: True proportion is = 0.5; 
    ##  Ha: True proportion is != 0.5 
    ##   
    ##  p-value = 0.0534

Note that we are creating our binary data within the `proptest` call.
The `proptest` function works with 0-1 numeric data, two-level factors,
or (as above) `TRUE`/`FALSE` data. Using the `exact` argument allows us
to choose what kind of test we run. In this case, the results are quite
similar.

Given two samples, we can also test whether two proportions are equal to
each other. There is no `exact` option for a two-sample test. Here we
test whether the proportion of men with LDL greater than 128 mg/dL is
the same as the proportion of women.

``` r
proptest(mri$ldl > 128, by = mri$sex)
```

    ## 
    ## Call:
    ## proptest(var1 = mri$ldl > 128, by = mri$sex)
    ## 
    ## Two-sample proportion test (approximate) :
    ##  
    ##               Group Obs Missing      Mean Std. Err.          95% CI
    ##    mri$sex = Female 369       4 0.5287671    0.0261  [0.4776, 0.58]
    ##      mri$sex = Male 366       6 0.3972222    0.0258 [0.3467, 0.448]
    ##          Difference 735      10 0.1315449    0.0367 [0.0596, 0.203]
    ## Summary:
    ## 
    ##  Ho: Difference in proportions = 0 
    ##  Ha: Difference in proportions != 0 
    ##  Z = 3.55 
    ##  p.value = 0.000383

The `proptesti` function is analogous to `ttesti` described above -
rather than providing data vectors, we can provide summary statistics in
the form of counts of successes out of a total number of trials. Here we
test whether the proportion of people with weight greater than 155 lbs
is equal to 0.6.

``` r
proptesti(sum(mri$weight > 155), length(mri$weight), exact = FALSE, null.hypoth= 0.6)
```

    ## 
    ## Call:
    ## proptesti(x1 = sum(mri$weight > 155), n1 = length(mri$weight), 
    ##     exact = FALSE, null.hypoth = 0.6)
    ## 
    ## One-sample proportion test (approximate)  :
    ##  
    ##  Variable Obs Mean  Std. Error 95% CI        
    ##  var1     735 0.533 0.0184     [0.497, 0.569]
    ## Summary:
    ## 
    ##  Ho: True proportion is = 0.6; 
    ##  Ha: True proportion is != 0.6 
    ##  Z = -3.69 
    ##  p.value = 0.000225

### Wilcoxon and Mann-Whitney

The Wilcoxon and Mann-Whitney tests, which use the “rank” of the given
variables, are nonparametric methods for analyzing the locations of the
underlying distributions that gave rise to a dataset. They are often
viewed as alternative to one- and two-sample t-tests, respectively.

Our function
[`wilcoxon()`](https://statdivlab.github.io/rigr/reference/wilcoxon.md)
takes one or two samples and performs either an approximate or exact
test of location. Since these tests are not based on the mean of the
data, the output looks slightly different from that of `ttest`. Here, we
perform a paired (matched) test on made-up data comparing individuals
with cystic fibrosis (CF) to health individuals.

``` r
## create the data
cf <- c(1153, 1132, 1165, 1460, 1162, 1493, 1358, 1453, 1185, 1824, 1793, 1930, 2075)
healthy <- c(996, 1080, 1182, 1452, 1634, 1619, 1140, 1123, 1113, 1463, 1632, 1614, 1836)

wilcoxon(cf, healthy, paired = TRUE)
```

    ## 
    ##  Wilcoxon signed rank test 
    ##          obs sum ranks expected
    ## positive  10        71     45.5
    ## negative   3        20     45.5
    ## zero       0         0      0.0
    ## all       13        91     91.0
    ##                             
    ## unadjusted variance   204.75
    ## adjustment for ties     0.00
    ## adjustment for zeroes   0.00
    ## adjusted variance     204.75
    ##                     H0 Ha       
    ## Hypothesized Median 0  two.sided
    ##   Test Statistic p-value 
    ## Z 1.7821         0.074735

This function can also provide a confidence interval for the median,
although unlike the Wilcoxon and Mann-Whitney tests, this confidence
interval is semiparametric rather than nonparametric.

``` r
wilcoxon(cf, healthy, paired = TRUE, conf.int = TRUE)
```

    ## 
    ##  Wilcoxon signed rank test 
    ##          obs sum ranks expected
    ## positive  10        71     45.5
    ## negative   3        20     45.5
    ## zero       0         0      0.0
    ## all       13        91     91.0
    ##                             
    ## unadjusted variance   204.75
    ## adjustment for ties     0.00
    ## adjustment for zeroes   0.00
    ## adjusted variance     204.75
    ##                     H0 Ha       
    ## Hypothesized Median 0  two.sided
    ##   Test Statistic p-value  CI           Point Estimate
    ## Z 1.7821         0.074735 [-27, 238.5] 117.5

Note that there is no version of `wilcoxon` using summary statistics,
since the test relies on the ranks of the observed data.
