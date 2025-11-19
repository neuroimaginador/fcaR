# Calculate Concept Separation

Computes the separation of each concept. Separation is defined as the
number of objects in a concept's extent that are NOT covered by any of
its *immediate* subconcepts (children).

## Usage

``` r
calculate_separation(lattice)
```

## Arguments

- lattice:

  A `ConceptLattice` object.

## Value

A numeric vector of separation values.
