# Equality in Sets and Concepts

Equality in Sets and Concepts

## Usage

``` r
C1 %==% C2
```

## Arguments

- C1:

  A `Set` or `Concept`

- C2:

  A `Set` or `Concept`

## Value

Returns `TRUE` if `C1` is equal to `C2`.

## Details

Both `C1` and `C2` must be of the same class.

## Examples

``` r
# Build two sparse sets
S <- Set$new(attributes = c("A", "B", "C"))
S$assign(A = 1)
T <- Set$new(attributes = c("A", "B", "C"))
T$assign(A = 1)

# Test whether S and T are equal
S %==% T
#> [1] TRUE
```
