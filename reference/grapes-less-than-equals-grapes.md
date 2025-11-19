# Partial Order in Sets and Concepts

Partial Order in Sets and Concepts

## Usage

``` r
C1 %<=% C2
```

## Arguments

- C1:

  A `Set` or `Concept`

- C2:

  A `Set` or `Concept`

## Value

Returns `TRUE` if concept `C1` is subconcept of `C2` or if set `C1` is
subset of `C2`.

## Details

Both `C1` and `C2` must be of the same class.

## Examples

``` r
# Build two sparse sets
S <- Set$new(attributes = c("A", "B", "C"))
S$assign(A = 1)
T <- Set$new(attributes = c("A", "B", "C"))
T$assign(A = 1, B = 1)

# Test whether S is subset of T
S %<=% T
#> [1] TRUE
```
