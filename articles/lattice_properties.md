# Lattice properties

``` r
library(fcaR)
```

## Introduction

Formal Concept Analysis (FCA) connects data analysis with Order Theory
and Lattice Theory. Beyond simply extracting concepts, it is often
useful to analyze the **algebraic structure** of the resulting Concept
Lattice.

This vignette introduces a new set of features in `fcaR`, **Algebraic
Properties:** Efficiently check if a concept lattice is distributive,
modular, semimodular, or atomic.

## 1. Checking lattice properties

The `ConceptLattice` class now provides methods to verify standard
lattice-theoretic properties. These checks are implemented in optimized
C++ for performance.

Let’s use the built-in `planets` dataset as an example:

``` r
fc <- FormalContext$new(planets)
fc$find_concepts()

# Check properties
print(paste("Is Distributive?", fc$concepts$is_distributive()))
#> [1] "Is Distributive? FALSE"
print(paste("Is Modular?",      fc$concepts$is_modular()))
#> [1] "Is Modular? FALSE"
print(paste("Is Semimodular?",  fc$concepts$is_semimodular()))
#> 'as(<ngCMatrix>, "dgCMatrix")' is deprecated.
#> Use 'as(., "dMatrix")' instead.
#> See help("Deprecated") and help("Matrix-deprecated").
#> [1] "Is Semimodular? FALSE"
print(paste("Is Atomic?",       fc$concepts$is_atomic()))
#> [1] "Is Atomic? TRUE"
```

### Understanding the properties

- **Distributive:** A lattice is distributive if the operations of join
  ($\vee$) and meet ($\land$) distribute over each other. Distributive
  lattices are isomorphic to rings of sets (Birkhoff’s Representation
  Theorem).
- **Modular:** A generalization of distributivity. In a modular lattice,
  if $x \leq z$, then $x \vee (y \land z) = (x \vee y) \land z$. All
  distributive lattices are modular.
- **Upper Semimodular:** If $x$ covers $x \land y$, then $x \vee y$
  covers $y$. This property ensures the lattice is *graded* (all maximal
  chains have the same length).
- **Atomic:** Every element (except the bottom) lies above some *atom*
  (an element that covers the bottom). In FCA, atoms often correspond to
  object concepts.

### Example: A non-distributive lattice ($M_{3}$)

The “Diamond” lattice ($M_{3}$) is the smallest non-distributive
lattice. Let’s create it manually to verify our checks.

``` r
# Context for M3 (The Diamond)
# 3 objects, 3 attributes. Objects have 2 attributes each.
I_m3 <- matrix(c(
  0, 1, 1,
  1, 0, 1,
  1, 1, 0
), nrow = 3, byrow = TRUE)

fc_m3 <- FormalContext$new(I_m3)
fc_m3$find_concepts()

# M3 is Modular but NOT Distributive
print(paste("M3 Distributive:", fc_m3$concepts$is_distributive()))
#> [1] "M3 Distributive: TRUE"
print(paste("M3 Modular:",      fc_m3$concepts$is_modular()))
#> [1] "M3 Modular: TRUE"
```

## 2. Arrow Relations

Arrow relations highlight the structural relationships between objects
and attributes in a binary formal context. They are particularly useful
for identifying irreducible elements and understanding the lattice
structure.

For a pair $(g,m) \notin I$: - $\left. g\swarrow m \right.$ iff
$g\prime$ is maximal among intents not containing $m$. -
$\left. g\nearrow m \right.$ iff $m\prime$ is maximal among extents not
containing $g$. - $\left. g\updownarrow m \right.$ iff both
$\left. g\swarrow m \right.$ and $\left. g\nearrow m \right.$.

In `fcaR`, you can compute these relations using the
`calculate_arrow_relations()` method. Once computed, the
[`print()`](https://rdrr.io/r/base/print.html) and `to_latex()` methods
will display the arrows:

``` r
# Use the planets dataset
fc <- FormalContext$new(planets)
fc$calculate_arrow_relations()

# The print method now shows the arrows
print(fc)
#> FormalContext with 9 objects and 7 attributes.
#>          small  medium  large  near  far  moon  no_moon  
#>  Mercury   X      ↙      ↙     X    ↙   ↕      X     
#>    Venus   X      ↙      ↙     X    ↙   ↕      X     
#>    Earth   X      ↙      ↙     X    ↕    X      ↕    
#>     Mars   X      ↙      ↙     X    ↕    X      ↕    
#>  Jupiter   ↕     ↕      X     ↙    X     X      ↙    
#>   Saturn   ↕     ↕      X     ↙    X     X      ↙    
#>   Uranus   ↕      X      ↕    ↙    X     X      ↙    
#>  Neptune   ↕      X      ↕    ↙    X     X      ↙    
#>    Pluto   X      ↕      ↕    ↕    X     X      ↙
```
