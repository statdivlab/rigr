# Create a Partial Formula

Creates a partial formula of the form `~var1 + var2`. The partial
formula can be named by adding an equals sign before the tilde.

## Usage

``` r
U(...)
```

## Arguments

- ...:

  partial formula of the form `~var1 + var2`.

## Value

A partial formula (potentially named) for use in
[`regress`](https://statdivlab.github.io/rigr/reference/regress.md).

## See also

[`regress`](https://statdivlab.github.io/rigr/reference/regress.md)

## Examples

``` r
# Reading in a dataset
data(mri)

# Create a named partial formula
U(ma=~male+age)
#> $ma
#> ~male + age
#> <environment: 0x55a64d11f6a8>
#> 

# Create an unnamed partial formula

U(~male+age)
#> $`~male + age`
#> ~male + age
#> <environment: 0x55a64d11f6a8>
#> 
```
