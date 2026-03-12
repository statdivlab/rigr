# Descriptive Statistics

Produces table of relevant descriptive statistics for an arbitrary
number of variables of class `integer`, `numeric`, `Surv`, `Date`, or
`factor`. Descriptive statistics can be obtained within strata, and the
user can specify that only a subset of the data be used. Descriptive
statistics include the count of observations, the count of cases with
missing values, the mean, standard deviation, geometric mean, minimum,
and maximum. The user can specify arbitrary quantiles to be estimated,
as well as specifying the estimation of proportions of observations
within specified ranges.

## Usage

``` r
descrip(
  ...,
  strata = NULL,
  subset = NULL,
  probs = c(0.25, 0.5, 0.75),
  geomInclude = FALSE,
  replaceZeroes = FALSE,
  restriction = Inf,
  above = NULL,
  below = NULL,
  labove = NULL,
  rbelow = NULL,
  lbetween = NULL,
  rbetween = NULL,
  interval = NULL,
  linterval = NULL,
  rinterval = NULL,
  lrinterval = NULL
)
```

## Arguments

- ...:

  an arbitrary number of variables for which descriptive statistics are
  desired. The arguments can be vectors, matrices, or lists. Individual
  columns of a matrix or elements of a list may be of class `numeric`,
  `factor`, `Surv`, or `Date`. Factor variables are converted to
  integers. Character vectors will be coerced to numeric. Variables may
  be of different lengths, unless `strata` or `subset` are non-`NULL`. A
  single `data.frame` or `tibble` may also be entered, in which case
  each variable in the object will be described.

- strata:

  a vector, matrix, or list of stratification variables. Descriptive
  statistics will be computed within strata defined by each unique
  combination of the stratification variables, as well as in the
  combined sample. If `strata` is supplied, all variables must be of
  that same length.

- subset:

  a vector indicating a subset to be used for all descriptive
  statistics. If `subset` is supplied, all variables must be of that
  same length.

- probs:

  a vector of probabilities between 0 and 1 indicating quantile
  estimates to be included in the descriptive statistics. Default is to
  compute 25th, 50th (median) and 75th percentiles.

- geomInclude:

  if not `FALSE` (the default), includes the geometric mean in the
  descriptive statistics.

- replaceZeroes:

  if not `FALSE` (the default), this indicates a value to be used in
  place of zeroes when computing a geometric mean. If `TRUE`, a value
  equal to one-half the lowest nonzero value is used. If a numeric value
  is supplied, that value is used for all variables.

- restriction:

  a value used for computing restricted means, standard deviations, and
  geometric means with censored time-to-event data. The default value of
  `Inf` will cause restrictions at the highest observation. Note that
  the same value is used for all variables of class `Surv`.

- above:

  a vector of values used to dichotomize variables. The descriptive
  statistics will include an estimate for each variable of the
  proportion of measurements with values greater than each element of
  `above`.

- below:

  a vector of values used to dichotomize variables. The descriptive
  statistics will include an estimate for each variable of the
  proportion of measurements with values less than each element of
  `below`.

- labove:

  a vector of values used to dichotomize variables. The descriptive
  statistics will include an estimate for each variable of the
  proportion of measurements with values greater than or equal to each
  element of `labove`.

- rbelow:

  a vector of values used to dichotomize variables. The descriptive
  statistics will include an estimate for each variable of the
  proportion of measurements with values less than or equal to each
  element of `rbelow`.

- lbetween:

  a vector of values with `-Inf` and `Inf` appended is used as cutpoints
  to categorize variables. The descriptive statistics will include an
  estimate for each variable of the proportion of measurements with
  values between successive elements of `lbetween`, with the left-hand
  endpoint included in each interval.

- rbetween:

  a vector of values with `-Inf` and `Inf` appended is used as cutpoints
  to categorize variables. The descriptive statistics will include an
  estimate for each variable of the proportion of measurements with
  values between successive elements of `rbetween`, with the right-hand
  endpoint included in each interval.

- interval:

  a two-column matrix of values in which each row is used to define
  intervals of interest to categorize variables. The descriptive
  statistics will include an estimate for each variable of the
  proportion of measurements with values between two elements in a row,
  with neither endpoint included in each interval.

- linterval:

  a two-column matrix of values in which each row is used to define
  intervals of interest to categorize variables. The descriptive
  statistics will include an estimate for each variable of the
  proportion of measurements with values between two elements in a row,
  with the left-hand endpoint included in each interval.

- rinterval:

  a two-column matrix of values in which each row is used to define
  intervals of interest to categorize variables. The descriptive
  statistics will include an estimate for each variable of the
  proportion of measurements with values between two elements in a row,
  with the right-hand endpoint included in each interval.

- lrinterval:

  a two-column matrix of values in which each row is used to define
  intervals of interest to categorize variables. The descriptive
  statistics will include an estimate for each variable of the
  proportion of measurements with values between two elements in a row,
  with both endpoints included in each interval.

## Value

An object of class `uDescriptives` is returned. Descriptive statistics
for each variable in the entire subsetted sample, as well as within each
stratum if any is defined, are contained in a matrix with rows
corresponding to variables and strata and columns corresponding to the
descriptive statistics. Descriptive statistics include

- N: the number of observations.

- Msng: the number of observations with missing values.

- Mean: the mean of the nonmissing observations (this is potentially a
  restricted mean for right-censored time-to-event data).

- Std Dev: the standard deviation of the nonmissing observations (this
  is potentially a restricted standard deviation for right-censored time
  to event data).

- Geom Mn: the geometric mean of the nonmissing observations (this is
  potentially a restricted geometric mean for right-censored time to
  event data). Nonpositive values in the variable will generate `NA`,
  unless `replaceZeroes` was specified.

- Min: the minimum value of the nonmissing observations (this is
  potentially restricted for right-censored time-to-event data).

- Quantiles: columns corresponding to the quantiles specified by `probs`
  (these are potentially restricted for right-censored time-to-event
  data).

- Max: the maximum value of the nonmissing observations (this is
  potentially restricted for right-censored time-to-event data).

- Proportions: columns corresponding to the proportions as specified by
  `above`, `below`, `labove`, `rbelow`, `lbetween`, `rbetween`,
  `interval`, `linterval`, `rinterval`, and `lrinterval`.

- restriction: the threshold for restricted means, standard deviations,
  and geometric means.

- firstEvent: the time of the first event for censored time-to-event
  variables.

- lastEvent: the time of the last event for censored time-to-event
  variables.

- isDate: an indicator that the variable is a `Date` object.

## Details

This function depends on the `survival` R package. You should execute
[`library(survival)`](https://github.com/therneau/survival) if that
library has not been previously installed. Quantiles are computed for
uncensored data using the default method in
[`quantile()`](https://rdrr.io/r/stats/quantile.html). For variables of
class `factor`, descriptive statistics will be computed using the
integer coding for factors. For variables of class `Surv`, estimated
proportions and quantiles will be computed from Kaplan-Meier estimates,
as will be restricted means, restricted standard deviations, and
restricted geometric means. For variables of class `Date`, estimated
proportions will be labeled using the Julian date since January 1, 1970.

## Examples

``` r
# Read in the data
data(mri) 

# Create the table 
descrip(mri)
#>                                                           N     Msng 
#>                                                   ptid:     735     0
#>                                                mridate:     735     0
#>                                                    age:     735     0
#>                                             sex_Female:     735     0
#>                                               sex_Male:     735     0
#>                                             race_Asian:     735     0
#>                                             race_Black:     735     0
#> race_Subject did not identify as White, Black or Asian:     735     0
#>                                             race_White:     735     0
#>                                                 weight:     735     0
#>                                                 height:     735     0
#>                                                packyrs:     735     1
#>                                                yrsquit:     735     0
#>                                                  alcoh:     735     0
#>                                                physact:     735     0
#>                                                    chf:     735     0
#>                                                    chd:     735     0
#>                                                 stroke:     735     0
#>                                               diabetes:     735     0
#>                                                genhlth:     735     0
#>                                                    ldl:     735    10
#>                                                    alb:     735     2
#>                                                    crt:     735     2
#>                                                    plt:     735     7
#>                                                    sbp:     735     0
#>                                                    aai:     735     9
#>                                                    fev:     735    10
#>                                                   dsst:     735    12
#>                                                atrophy:     735     0
#>                                                  whgrd:     735     1
#>                                                 numinf:     735     0
#>                                                 volinf:     735     1
#>                                                obstime:     735     0
#>                                                  death:     735     0
#>                                                           Mean       Std Dev  
#>                                                   ptid:     368.0      212.3  
#>                                                mridate:   1992-05-09   111.9  
#>                                                    age:     74.57      5.451  
#>                                             sex_Female:     0.5020     0.5003 
#>                                               sex_Male:     0.4980     0.5003 
#>                                             race_Asian:    0.06395     0.2448 
#>                                             race_Black:     0.1415     0.3488 
#> race_Subject did not identify as White, Black or Asian:    0.01633     0.1268 
#>                                             race_White:     0.7782     0.4157 
#>                                                 weight:     159.9      30.74  
#>                                                 height:     165.8      9.710  
#>                                                packyrs:     19.60      27.11  
#>                                                yrsquit:     9.661      14.10  
#>                                                  alcoh:     2.109      4.852  
#>                                                physact:     1.922      2.052  
#>                                                    chf:    0.05578     0.2297 
#>                                                    chd:     0.3347     0.6862 
#>                                                 stroke:     0.2367     0.6207 
#>                                               diabetes:     0.1075     0.3099 
#>                                                genhlth:     2.588      0.9382 
#>                                                    ldl:     125.8      33.60  
#>                                                    alb:     3.994      0.2690 
#>                                                    crt:     1.064      0.3030 
#>                                                    plt:     246.0      65.80  
#>                                                    sbp:     131.1      19.66  
#>                                                    aai:     1.103      0.1828 
#>                                                    fev:     2.207      0.6875 
#>                                                   dsst:     41.06      12.71  
#>                                                atrophy:     35.98      12.92  
#>                                                  whgrd:     2.007      1.410  
#>                                                 numinf:     0.6109     0.9895 
#>                                                 volinf:     3.223      17.36  
#>                                                obstime:      1804      392.3  
#>                                                  death:     0.1810     0.3852 
#>                                                           Min        25%       
#>                                                   ptid:     1.000      184.5   
#>                                                mridate:   1991-10-19 1992-01-10
#>                                                    age:     65.00      71.00   
#>                                             sex_Female:      NA         NA     
#>                                               sex_Male:      NA         NA     
#>                                             race_Asian:      NA         NA     
#>                                             race_Black:      NA         NA     
#> race_Subject did not identify as White, Black or Asian:      NA         NA     
#>                                             race_White:      NA         NA     
#>                                                 weight:     74.00      138.5   
#>                                                 height:     139.0      158.0   
#>                                                packyrs:     0.0000     0.0000  
#>                                                yrsquit:     0.0000     0.0000  
#>                                                  alcoh:     0.0000     0.0000  
#>                                                physact:     0.0000     0.5538  
#>                                                    chf:      NA         NA     
#>                                                    chd:     0.0000     0.0000  
#>                                                 stroke:     0.0000     0.0000  
#>                                               diabetes:      NA         NA     
#>                                                genhlth:     1.000      2.000   
#>                                                    ldl:     11.00      102.0   
#>                                                    alb:     3.200      3.800   
#>                                                    crt:     0.5000     0.9000  
#>                                                    plt:     92.00      201.8   
#>                                                    sbp:     78.00      118.0   
#>                                                    aai:     0.3171     1.027   
#>                                                    fev:     0.4083     1.745   
#>                                                   dsst:     0.0000     32.00   
#>                                                atrophy:     5.000      27.00   
#>                                                  whgrd:     0.0000     1.000   
#>                                                 numinf:     0.0000     0.0000  
#>                                                 volinf:     0.0000     0.0000  
#>                                                obstime:     68.00       1837   
#>                                                  death:      NA         NA     
#>                                                           Mdn        75%       
#>                                                   ptid:     368.0      551.5   
#>                                                mridate:   1992-07-05 1992-08-12
#>                                                    age:     74.00      78.00   
#>                                             sex_Female:      NA         NA     
#>                                               sex_Male:      NA         NA     
#>                                             race_Asian:      NA         NA     
#>                                             race_Black:      NA         NA     
#> race_Subject did not identify as White, Black or Asian:      NA         NA     
#>                                             race_White:      NA         NA     
#>                                                 weight:     158.0      179.0   
#>                                                 height:     165.9      173.2   
#>                                                packyrs:     6.500      33.75   
#>                                                yrsquit:     0.0000     18.50   
#>                                                  alcoh:    0.01920     1.144   
#>                                                physact:     1.312      2.513   
#>                                                    chf:      NA         NA     
#>                                                    chd:     0.0000     0.0000  
#>                                                 stroke:     0.0000     0.0000  
#>                                               diabetes:      NA         NA     
#>                                                genhlth:     3.000      3.000   
#>                                                    ldl:     125.0      147.0   
#>                                                    alb:     4.000      4.200   
#>                                                    crt:     1.000      1.200   
#>                                                    plt:     239.0      285.0   
#>                                                    sbp:     130.0      142.0   
#>                                                    aai:     1.112      1.207   
#>                                                    fev:     2.158      2.649   
#>                                                   dsst:     40.00      50.00   
#>                                                atrophy:     35.00      44.00   
#>                                                  whgrd:     2.000      3.000   
#>                                                 numinf:     0.0000     1.000   
#>                                                 volinf:     0.0000    0.09420  
#>                                                obstime:      1879       2044   
#>                                                  death:      NA         NA     
#>                                                           Max       
#>                                                   ptid:     735.0   
#>                                                mridate:   1992-10-12
#>                                                    age:     99.00   
#>                                             sex_Female:      NA     
#>                                               sex_Male:      NA     
#>                                             race_Asian:      NA     
#>                                             race_Black:      NA     
#> race_Subject did not identify as White, Black or Asian:      NA     
#>                                             race_White:      NA     
#>                                                 weight:     264.0   
#>                                                 height:     190.5   
#>                                                packyrs:     240.0   
#>                                                yrsquit:     56.00   
#>                                                  alcoh:     35.00   
#>                                                physact:     13.81   
#>                                                    chf:      NA     
#>                                                    chd:     2.000   
#>                                                 stroke:     2.000   
#>                                               diabetes:      NA     
#>                                                genhlth:     5.000   
#>                                                    ldl:     247.0   
#>                                                    alb:     5.000   
#>                                                    crt:     4.000   
#>                                                    plt:     539.0   
#>                                                    sbp:     210.0   
#>                                                    aai:     1.728   
#>                                                    fev:     4.471   
#>                                                   dsst:     82.00   
#>                                                atrophy:     84.00   
#>                                                  whgrd:     9.000   
#>                                                 numinf:     5.000   
#>                                                 volinf:     197.0   
#>                                                obstime:      2159   
#>                                                  death:      NA     
```
