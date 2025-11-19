# Convert Named Vector to Set

Convert Named Vector to Set

## Usage

``` r
as_Set(A)
```

## Arguments

- A:

  A named vector or matrix to build a new `Set`.

## Value

A `Set` object.

## Examples

``` r
A <- c(a = 0.1, b = 0.2, p = 0.3, q = 0)
as_Set(A)
#> {a [0.1], b [0.2], p [0.3]}
```
