
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fcaR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/fcaR)](https://cran.r-project.org/package=fcaR)
[![Travis build
status](https://travis-ci.org/neuroimaginador/fcaR.svg?branch=master)](https://travis-ci.org/neuroimaginador/fcaR)
[![Codecov test
coverage](https://codecov.io/gh/neuroimaginador/fcaR/branch/master/graph/badge.svg)](https://codecov.io/gh/neuroimaginador/fcaR?branch=master)
<!-- badges: end -->

The goal of fcaR is to provide FCA tools inside the R environment.

## Installation

The development version of this package can be installed with

    remotes::install_github("neuroimaginador/fcaR")

## Example in Fuzzy Formal Context

Example from
[here](https://www.sciencedirect.com/science/article/pii/S1877705812021418)

``` r

library(fcaR)

# Table 2 in the paper

objects <- paste0("O", 1:6)
n_objects <- length(objects)

attributes <- paste0("P", 1:6)
n_attributes <- length(attributes)

I <- matrix(data = c(0, 1, 0.5, 0, 0, 0.5,
                     1, 1, 0.5, 0, 0, 0,
                     0.5, 1, 0, 0, 1, 0,
                     0.5, 0, 0, 1, 0.5, 0,
                     1, 0, 0, 0.5, 0, 0,
                     0, 0, 1, 0, 0, 0),
            nrow = n_objects,
            byrow = FALSE)

colnames(I) <- attributes
rownames(I) <- objects

print(I)
#>     P1  P2  P3  P4  P5 P6
#> O1 0.0 1.0 0.5 0.5 1.0  0
#> O2 1.0 1.0 1.0 0.0 0.0  0
#> O3 0.5 0.5 0.0 0.0 0.0  1
#> O4 0.0 0.0 0.0 1.0 0.5  0
#> O5 0.0 0.0 1.0 0.5 0.0  0
#> O6 0.5 0.0 0.0 0.0 0.0  0
```

``` r
# By default, the set of grades are the unique
# values in the (fuzzy) formal context
# Let us build the formal_context object
fc <- formal_context$new(I)

# Compute all concepts
concept_list <- fc$compute_concepts()

# And plot the concept lattice
fc$plot_lattice()
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

We can also extract implications from the formal context:

``` r
# Extract implications
fc$extract_implications_concepts()

# Which implications have been extracted
fc$implications
#> Rule 1: {P6 [0.5]} -> {P1 [0.5], P2 [0.5], P6 [1]}
#> Rule 2: {P5 [0.5]} -> {P4 [0.5]}
#> Rule 3: {P3 [0.5], P4 [0.5], P5 [0.5]} -> {P2 [1], P5 [1]}
#> Rule 4: {P3 [0.5], P4 [1]} -> {P3 [1]}
#> Rule 5: {P2 [0.5], P4 [0.5]} -> {P2 [1], P3 [0.5], P5 [1]}
#> Rule 6: {P2 [0.5], P3 [0.5]} -> {P2 [1]}
#> Rule 7: {P2 [1], P3 [1], P4 [0.5], P5 [1]} -> {P4 [1]}
#> Rule 8: {P1 [0.5], P4 [0.5]} -> {P1 [1], P2 [1], P3 [1], P4 [1], P5 [1], P6 [1]}
#> Rule 9: {P1 [0.5], P3 [0.5]} -> {P1 [1], P2 [1], P3 [1]}
#> Rule 10: {P1 [0.5], P2 [1]} -> {P1 [1]}
#> Rule 11: {P1 [1], P2 [0.5]} -> {P2 [1]}
#> Rule 12: {P1 [1], P2 [1], P3 [1], P6 [1]} -> {P4 [1], P5 [1]}

# Reduce the number of implications using two simple
# rules. The algorithm applies the specified rules
# in batches, if the number of rules is high.
fc$implications$batch_apply(rules = c("composition",
                                      "generalization"))
#> Using parallel execution
#> Processing batch
#> --> composition : from 12 to 12 in 0.002 secs. 
#> --> generalization : from 12 to 12 in 0.007 secs. 
#> Batch took 0.011 secs.

# Reduced set of implications
fc$implications
#> Rule 1: {P3 [0.5], P4 [1]} -> {P3 [1]}
#> Rule 2: {P2 [1], P3 [1], P4 [0.5], P5 [1]} -> {P4 [1]}
#> Rule 3: {P1 [0.5], P2 [1]} -> {P1 [1]}
#> Rule 4: {P1 [1], P2 [0.5]} -> {P2 [1]}
#> Rule 5: {P6 [0.5]} -> {P1 [0.5], P2 [0.5], P6 [1]}
#> Rule 6: {P3 [0.5], P4 [0.5], P5 [0.5]} -> {P2 [1], P5 [1]}
#> Rule 7: {P1 [0.5], P3 [0.5]} -> {P1 [1], P2 [1], P3 [1]}
#> Rule 8: {P2 [0.5], P3 [0.5]} -> {P2 [1]}
#> Rule 9: {P1 [0.5], P4 [0.5]} -> {P1 [1], P2 [1], P3 [1], P4 [1], P5 [1], P6 [1]}
#> Rule 10: {P1 [1], P2 [1], P3 [1], P6 [1]} -> {P4 [1], P5 [1]}
#> Rule 11: {P5 [0.5]} -> {P4 [0.5]}
#> Rule 12: {P2 [0.5], P4 [0.5]} -> {P2 [1], P3 [0.5], P5 [1]}

# We can obtain the support of both implications and concepts
fc$get_implication_support()
#>  [1] 0.0000000 0.0000000 0.1666667 0.1666667 0.1666667 0.1666667 0.1666667
#>  [8] 0.3333333 0.0000000 0.0000000 0.3333333 0.1666667
fc$get_concept_support()
#>  [1] 0.8333333 0.5000000 0.3333333 0.1666667 0.1666667 0.1666667 0.0000000
#>  [8] 0.5000000 0.3333333 0.3333333 0.1666667 0.0000000 0.5000000 0.3333333
#> [15] 0.3333333 0.1666667 0.1666667 0.0000000 0.5000000 0.3333333 0.1666667
#> [22] 0.1666667 0.1666667 0.0000000 0.1666667 0.0000000
```

## Example in Crisp Formal Context

``` r

library(fcaR)

# Define objects and attributes
objects <- paste0(1:5)
n_objects <- length(objects)

attributes <- letters[1:6]
n_attributes <- length(attributes)

# The formal context is just a simple matrix
I <- matrix(data = c(1, 1, 1, 0, 1,
                     1, 0, 1, 1, 0,
                     0, 0, 1, 0, 0,
                     0, 0, 1, 1, 0,
                     0, 0, 0, 1, 0,
                     0, 0, 1, 1, 0),
            nrow = n_objects,
            byrow = FALSE)

colnames(I) <- attributes
rownames(I) <- objects

print(I)
#>   a b c d e f
#> 1 1 1 0 0 0 0
#> 2 1 0 0 0 0 0
#> 3 1 1 1 1 0 1
#> 4 0 1 0 1 1 1
#> 5 1 0 0 0 0 0
```

``` r
# By default, the set of grades are the unique
# values in the (fuzzy) formal context
# Let us build the formal_context object
fc <- formal_context$new(I)

# Compute all concepts
concept_list <- fc$compute_concepts()

# And plot the concept lattice
fc$plot_lattice()
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

We can also extract implications from the formal context:

``` r
# Extract implications
fc$extract_implications_concepts()

# Which implications have been extracted
fc$implications
#> Rule 1: {f [1]} -> {b [1], d [1]}
#> Rule 2: {e [1]} -> {b [1], d [1], f [1]}
#> Rule 3: {d [1]} -> {b [1], f [1]}
#> Rule 4: {c [1]} -> {a [1], b [1], d [1], f [1]}
#> Rule 5: {a [1], b [1], d [1], f [1]} -> {c [1]}

# Reduce the number of implications using two simple
# rules. The algorithm applies the specified rules
# in batches, if the number of rules is high.
fc$implications$batch_apply(rules = c("composition",
                                      "generalization"))
#> Using parallel execution
#> Processing batch
#> --> composition : from 5 to 5 in 0.001 secs. 
#> --> generalization : from 5 to 5 in 0.002 secs. 
#> Batch took 0.005 secs.

# Reduced set of implications
fc$implications
#> Rule 1: {f [1]} -> {b [1], d [1]}
#> Rule 2: {d [1]} -> {b [1], f [1]}
#> Rule 3: {a [1], b [1], d [1], f [1]} -> {c [1]}
#> Rule 4: {e [1]} -> {b [1], d [1], f [1]}
#> Rule 5: {c [1]} -> {a [1], b [1], d [1], f [1]}

# We can obtain the support of both implications and concepts
fc$get_implication_support()
#> [1] 0.4 0.4 0.2 0.2 0.2
fc$get_concept_support()
#> [1] 0.8 0.6 0.4 0.2 0.8 0.4 0.2 0.0
```
