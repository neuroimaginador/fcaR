# Generate a Random Distributive Context

Generates a random formal context that is guaranteed to produce a
**Distributive Concept Lattice**.

It relies on Birkhoff's Representation Theorem: The lattice of order
ideals of a Poset is always distributive. The context is constructed
such that objects and attributes are the elements of the poset, and the
incidence relation is \\g I m \iff \neg(g \ge m)\\.

## Usage

``` r
RandomDistributiveContext(n_elements, density = 0.1)
```

## Arguments

- n_elements:

  Number of elements in the underlying Poset.

- density:

  Probability of an order relation \\a \le b\\.

## Value

A `FormalContext`.
