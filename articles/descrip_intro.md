# Descriptive statistics in rigr

A key feature of many exploratory analyses is obtaining descriptive
statistics for multiple variables. In the `rigr` package, we provide a
function
[`descrip()`](https://statdivlab.github.io/rigr/reference/descrip.md)
with improved output for descriptive statistics for an arbitrary number
of variables. Key features include the ability to easily compute summary
measures on strata or subsets of the variables specified. We go through
examples making use of these key features below.

## Descriptive statistics with `descrip`

Throughout our examples, we’ll use the `fev` dataset. This dataset is
included in the `rigr` package; see its documentation by running
[`?fev`](https://statdivlab.github.io/rigr/reference/fev.md).

``` r
## Preparing our R session
library(rigr)
```

    ## rigr version 1.0.8: Regression, Inference, and General Data Analysis Tools in R

``` r
data(fev)
```

First, we can obtain default descriptive statistics for the dataset
simply by running
[`descrip()`](https://statdivlab.github.io/rigr/reference/descrip.md).

``` r
descrip(fev)
```

    ##               N     Msng  Mean      Std Dev   Min       25%       Mdn      
    ##     seqnbr:     654     0   327.5     188.9     1.000     164.2     327.5  
    ##     subjid:     654     0   37170     23691     201.0     15811     36071  
    ##        age:     654     0   9.931     2.954     3.000     8.000     10.00  
    ##        fev:     654     0   2.637     0.8671    0.7910    1.981     2.547  
    ##     height:     654     0   61.14     5.704     46.00     57.00     61.50  
    ## sex_female:     654     0   0.4862    0.5002     NA        NA        NA    
    ##   sex_male:     654     0   0.5138    0.5002     NA        NA        NA    
    ##   smoke_no:     654     0   0.9006    0.2994     NA        NA        NA    
    ##  smoke_yes:     654     0  0.09939    0.2994     NA        NA        NA    
    ##               75%       Max      
    ##     seqnbr:     490.8     654.0  
    ##     subjid:     53638     90001  
    ##        age:     12.00     19.00  
    ##        fev:     3.118     5.793  
    ##     height:     65.50     74.00  
    ## sex_female:      NA        NA    
    ##   sex_male:      NA        NA    
    ##   smoke_no:      NA        NA    
    ##  smoke_yes:      NA        NA

Since we input a dataframe, we can see that all variables have the same
number of elements given in the `N` column. None of our variables have
any missing values, as seen in the `Msng` column.

Rather than specifying the whole dataframe, if we are interested in only
the variables `fev` and `height`, we can input only those two vectors
into the
[`descrip()`](https://statdivlab.github.io/rigr/reference/descrip.md)
function, as below.

``` r
descrip(fev$fev, fev$height)
```

    ##               N     Msng  Mean      Std Dev   Min       25%       Mdn      
    ##    fev$fev:     654     0   2.637     0.8671    0.7910    1.981     2.547  
    ## fev$height:     654     0   61.14     5.704     46.00     57.00     61.50  
    ##               75%       Max      
    ##    fev$fev:     3.118     5.793  
    ## fev$height:     65.50     74.00

## Descriptive statistics for strata

Suppose we wish to obtain descriptive statistics of the `fev` and
`height` variables, stratified by smoking status. To do this, we can use
the `strata` parameter in `descrip`:

``` r
descrip(fev$fev, fev$height, strata = fev$smoke)
```

    ##                         N     Msng  Mean      Std Dev   Min       25%      
    ##    fev$fev:  All          654     0   2.637     0.8671    0.7910    1.981  
    ##    fev$fev:    Str  no    589     0   2.566     0.8505    0.7910    1.920  
    ##    fev$fev:    Str  yes    65     0   3.277     0.7500    1.694     2.795  
    ## fev$height:  All          654     0   61.14     5.704     46.00     57.00  
    ## fev$height:    Str  no    589     0   60.61     5.672     46.00     57.00  
    ## fev$height:    Str  yes    65     0   65.95     3.193     58.00     63.50  
    ##                         Mdn       75%       Max      
    ##    fev$fev:  All          2.547     3.118     5.793  
    ##    fev$fev:    Str  no    2.465     3.048     5.793  
    ##    fev$fev:    Str  yes   3.169     3.751     4.872  
    ## fev$height:  All          61.50     65.50     74.00  
    ## fev$height:    Str  no    61.00     64.50     74.00  
    ## fev$height:    Str  yes   66.00     68.00     72.00

In the output, we can see that overall descriptive statistics, as well
as descriptive statistics for each stratum (smoke = 1, smoke = 2) are
returned in the table.

## Descriptive statistics for subsets

Now suppose we only want descriptive statistics for height and FEV for
individuals over the age of 10. We first create an indicator variable
for `age > 10` *outside* of the
[`descrip()`](https://statdivlab.github.io/rigr/reference/descrip.md)
function, and then give this variable to the `subset` parameter.

``` r
greater_10 <- ifelse(fev$age > 10, 1, 0)
descrip(fev$fev, fev$height, subset = greater_10)
```

    ##               N     Msng  Mean      Std Dev   Min       25%       Mdn      
    ##    fev$fev:     264     0   1.708     0.0000    1.708     1.708     1.708  
    ## fev$height:     264     0   57.00     0.0000    57.00     57.00     57.00  
    ##               75%       Max      
    ##    fev$fev:     1.708     1.708  
    ## fev$height:     57.00     57.00

## Above/Below

Suppose we want to know the proportion of individuals with FEV greater
than 2, stratified by smoking status. We can use the `strata` argument
as before, in addition to the `above` parameter to obtain this set of
descriptive statistics:

``` r
descrip(fev$fev, strata = fev$smoke, above = 2)
```

    ##                      N     Msng  Mean      Std Dev   Min       25%      
    ## fev$fev:  All          654     0   2.637     0.8671    0.7910    1.981  
    ## fev$fev:    Str  no    589     0   2.566     0.8505    0.7910    1.920  
    ## fev$fev:    Str  yes    65     0   3.277     0.7500    1.694     2.795  
    ##                      Mdn       75%       Max       Pr>2     
    ## fev$fev:  All          2.547     3.118     5.793     0.7446 
    ## fev$fev:    Str  no    2.465     3.048     5.793     0.7199 
    ## fev$fev:    Str  yes   3.169     3.751     4.872     0.9692

From the output, we can see that 96.92% of the individuals in this
dataset who smoke (smoking status 1) had an FEV greater than 2 L/sec,
and 71.99% of the individuals in this dataset who were nonsmokers had an
FEV greater than 2 L/sec. 
