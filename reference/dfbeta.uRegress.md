# Calculate dfbeta from `uRegress` objects

Extracts dfbeta from `uRegress` objects by relying on functionality from
the `stats` package. Note that `dfbeta` and `dfbetas` are not the same
(`dfbetas` are less than the `dfbeta` values by a scaling factor that
reflects both the leverage of the observation in question and the
residual model error).

## Usage

``` r
# S3 method for class 'uRegress'
dfbeta(model, ...)
```

## Arguments

- model:

  an object of class `uRegress`, as returned by
  [regress](https://statdivlab.github.io/rigr/reference/regress.md).

- ...:

  other arguments to pass to
  [`stats::dfbeta`](https://rdrr.io/r/stats/influence.measures.html)

## Value

a matrix of dfbeta values, with a row for each observation and a column
for each model coefficient
