# Extract standardized residuals from `uRegress` objects

Extracts standardized residuals from `uRegress` objects by relying on
functionality from the `stats` package.

## Usage

``` r
# S3 method for class 'uRegress'
rstandard(model, ...)
```

## Arguments

- model:

  an object of class `uRegress`, as returned by
  [regress](https://statdivlab.github.io/rigr/reference/regress.md).

- ...:

  other arguments to pass to `residuals.uRegress`

## Value

a vector of standardized residuals
