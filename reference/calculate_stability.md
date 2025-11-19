# Calculate Concept Stability

Calculates the intensional stability of each concept.

Calculates the intensional stability of each concept in the lattice
directly from the sparse matrix representation. Stability measures the
probability that a concept is preserved when a random subset of objects
is removed.

## Usage

``` r
calculate_stability(extents)

calculate_stability(extents)
```

## Arguments

- extents:

  A `SparseSet` object or a sparse matrix (`CsparseMatrix`) representing
  concept extents (columns are concepts).

## Value

A numeric vector.

A numeric vector with stability values in \\\[0, 1\]\\.
