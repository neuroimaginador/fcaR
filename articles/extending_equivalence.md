# Extending fcaR: Equivalence Rules for Implications

## Introduction

In this vignette, we present a way to extend the functionalities
provided in the `fcaR` package: define new operations on an
`ImplicationSet`.

First, we load the `fcaR` package by:

``` r
library(fcaR)
```

Let us use the `planets` dataset included in the package:

``` r
fc <- FormalContext$new(planets)
fc$find_implications()
```

## The Registry

In `fcaR`, we have decided to use a `registry` from the `registry`
package to store the operations that can be performed on an
`ImplicationSet`. Currently, its purpose is to store equivalence rules,
that is, methods that obtain equivalent `ImplicationSet`s from one
given.

This registry is called `equivalencesRegistry` and one can inspect its
contents by:

``` r
equivalencesRegistry$get_entry_names()
#> [1] "Composition"          "Generalization"       "Reduction"           
#> [4] "Simplification"       "Right Simplification" "Reorder"
```

These names correspond to the methods that are added to the registry by
default, and are used to index those methods. Every method is
accompanied by a description, so we can see its definition:

``` r
equivalencesRegistry$get_entry("Composition")
#>      method Composition
#>         fun <<function>>
#> description A -> B and A -> C equivalent to A -> BC
```

We can even use abbreviated names to refer to the method:

``` r
equivalencesRegistry$get_entry("comp")
#>      method Composition
#>         fun <<function>>
#> description A -> B and A -> C equivalent to A -> BC
```

## Use of the Rules

As explained in the vignette corresponding to `ImplicationSet`s, we can
use any of these methods by using the `apply_rules()` method in the
`ImplicationSet`:

``` r
fc$implications$apply_rules(c("comp", "simp"))
```

## Definition of New Equivalence Rules

The way to extend the functionality in `fcaR` is to define new
equivalence operators and include them in the registry.

In order to add a new method, we use:

``` r
equivalencesRegistry$set_entry(
  method = "Method name",
  fun = method_function,
  description = "Method description"
)
```

where `method_function()` must be a function with the following scheme:

``` r
method_function <- function(LHS, RHS, attributes) {
  # LHS and RHS are the sparse matrices of the left-hand and
  # right-hand sides of the implications
  # attributes is the vector of attribute names
  # The three arguments are mandatory

  # Perform operations on LHS and RHS
  # ...

  # Must return a list with two components: lhs and rhs
  return(list(
    lhs = LHS,
    rhs = RHS
  ))
}
```

The `method_function()` function must be defined before adding the
method to the registry. Once the method is added, it can be executed by
using the corresponding call to `apply_rules()`.

## An Example

Let us define an operator which randomly reorders the implications.
Evidently, this operation provides an equivalent `ImplicationSet`.

In this case, we begin by defining the method function:

``` r
random_reorder <- function(LHS, RHS, attributes) {
  # Remember: attributes are in rows, implications are
  # in columns.
  # Random order for columns:
  o <- sample(ncol(LHS), ncol(LHS))

  # Return the reordered implications
  return(list(
    lhs = LHS[, o],
    rhs = RHS[, o]
  ))
}
```

Once we have defined the function, we add the method to the registry:

``` r
equivalencesRegistry$set_entry(
  method = "Randomize",
  fun = random_reorder,
  description = "Randomize the order of the implications."
)
```

If we inspect the registry, we obtain the list of the methods, including
the one we have just inserted:

``` r
equivalencesRegistry$get_entry_names()
#> [1] "Composition"          "Generalization"       "Reduction"           
#> [4] "Simplification"       "Right Simplification" "Reorder"             
#> [7] "Randomize"
```

We can apply the new method:

``` r
# Original implications
fc$implications
#> Implication set with 10 implications.
#> Rule 1: {no_moon} -> {small, near}
#> Rule 2: {far} -> {moon}
#> Rule 3: {near} -> {small}
#> Rule 4: {large} -> {far, moon}
#> Rule 5: {medium} -> {far, moon}
#> Rule 6: {medium, large, far, moon} -> {small, near, no_moon}
#> Rule 7: {small, near, moon, no_moon} -> {medium, large, far}
#> Rule 8: {small, near, far, moon} -> {medium, large, no_moon}
#> Rule 9: {small, large, far, moon} -> {medium, near, no_moon}
#> Rule 10: {small, medium, far, moon} -> {large, near, no_moon}
```

``` r
# Apply the randomize method
fc$implications$apply_rules("randomize")
```

``` r
# Reordered implications
fc$implications
#> Implication set with 10 implications.
#> Rule 1: {small, near, moon, no_moon} -> {medium, large, far}
#> Rule 2: {medium} -> {far, moon}
#> Rule 3: {medium, large, far, moon} -> {small, near, no_moon}
#> Rule 4: {small, medium, far, moon} -> {large, near, no_moon}
#> Rule 5: {large} -> {far, moon}
#> Rule 6: {no_moon} -> {small, near}
#> Rule 7: {small, large, far, moon} -> {medium, near, no_moon}
#> Rule 8: {small, near, far, moon} -> {medium, large, no_moon}
#> Rule 9: {near} -> {small}
#> Rule 10: {far} -> {moon}
```
