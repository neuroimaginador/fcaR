# Generate a Random Distributive Context

Generates a random formal context that is guaranteed to produce a
**Distributive Concept Lattice**.

It relies on Birkhoff's Representation Theorem: The lattice of order
ideals of a Poset is always distributive.

## Usage

``` r
RandomDistributiveContext(n_elements, density = 0.1)
```

## Arguments

- n_elements:

  Number of elements in the underlying Poset. The resulting context will
  have `n_elements` objects and attributes.

- density:

  Probability of an order relation \\a \le b\\.

## Value

A `FormalContext`.
