# Compute bonds via MCIS (backtracking on pre-computed concepts)

Computes bonds using Algorithm 1 from "Computing bonds between formal
contexts", optimized with FastBitset in a unified C++ solver.
Pre-computes concepts (extents of C1 and intents of C2) in R, then
passes them to C++ for the backtracking search.

## Usage

``` r
bonds_mcis(fc1, fc2, verbose = FALSE)
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
