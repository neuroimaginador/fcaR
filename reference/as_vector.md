# Convert Set to vector

Convert Set to vector

## Usage

``` r
as_vector(v)
```

## Arguments

- v:

  A `Set` to convert to vector.

## Value

A vector.

## Examples

``` r
A <- c(a = 0.1, b = 0.2, p = 0.3, q = 0)
v <- as_Set(A)
A2 <- as_vector(v)
all(A == A2)
#> [1] TRUE
```
