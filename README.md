
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

## Example

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
grades_set <- sort(unique(as.vector(I)))

# Let us build the formal_context object
fc <- formal_context$new(I, grades_set)

# Compute all concepts
concept_list <- fc$get_concepts()
#> First concept:
#> {}
#> New concept:
#> {"b"}
#> New concept:
#> {"b", "d", "f"}
#> New concept:
#> {"b", "d", "e", "f"}
#> New concept:
#> {"a"}
#> New concept:
#> {"a", "b"}
#> New concept:
#> {"a", "b", "c", "d", "f"}
#> New concept:
#> {"a", "b", "c", "d", "e", "f"}

# And plot the concept lattice
fc$plot()
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />
