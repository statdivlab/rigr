# Calculate the hat-values (leverages) from `uRegress` objects

Extracts hat-values (leverages) from `uRegress` objects by relying on
functionality from the `stats` package.

## Usage

``` r
# S3 method for class 'uRegress'
hatvalues(model, ...)
```

## Arguments

- model:

  an object of class `uRegress`, as returned by
  [regress](https://statdivlab.github.io/rigr/reference/regress.md).

- ...:

  other arguments to pass to
  [`stats::hatvalues`](https://rdrr.io/r/stats/influence.measures.html)

## Value

a vector of hat-values (leverages)
