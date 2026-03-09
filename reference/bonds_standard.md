# Compute bonds via standard implication-based method

Computes bonds using implication merging and closed-set enumeration.
Uses a FastBitset-optimized C++ implementation.

## Usage

``` r
bonds_standard(fc1, fc2, verbose = FALSE)
```

## Arguments

- fc1:

  (`FormalContext`) The first formal context.

- fc2:

  (`FormalContext`) The second formal context.

- verbose:

  (`logical`) Print progress info.

## Value

A `BondLattice` object.
