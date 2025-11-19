# Intersection (Logical AND) of Fuzzy Sets

Intersection (Logical AND) of Fuzzy Sets

## Usage

``` r
S1 %&% S2
```

## Arguments

- S1:

  A `Set`

- S2:

  A `Set`

## Value

Returns the intersection of `S1` and `S2`.

## Details

Both `S1` and `S2` must be Sets.

## Examples

``` r
# Build two sparse sets
S <- Set$new(attributes = c("A", "B", "C"))
S$assign(A = 1, B = 1)
T <- Set$new(attributes = c("A", "B", "C"))
T$assign(A = 1, C = 1)

# Intersection
S %&% T
#> {A}
```
