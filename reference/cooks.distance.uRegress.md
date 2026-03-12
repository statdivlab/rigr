# Calculate Cook's distances from `uRegress` objects

Extracts Cook's distances from `uRegress` objects by relying on
functionality from the `stats` package.

## Usage

``` r
# S3 method for class 'uRegress'
cooks.distance(model, ...)
```

## Arguments

- model:

  an object of class `uRegress`, as returned by
  [regress](https://statdivlab.github.io/rigr/reference/regress.md).

- ...:

  other arguments to pass to
  [`stats::cooks.distance`](https://rdrr.io/r/stats/influence.measures.html)

## Value

a vector of Cook's distances
