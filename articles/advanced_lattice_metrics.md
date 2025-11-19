# Advanced lattice metrics

``` r
library(fcaR)
```

## Introduction

Beyond the standard **support** (frequency), `fcaR` provides advanced
metrics to analyze the quality and robustness of formal concepts,
especially in fuzzy settings.

- **Stability:** Measures the robustness of a concept against noise.
- **Separation:** Measures the specificity of the objects covered by a
  concept.
- **Fuzzy Density:** Measures the cohesion of the concept in the
  original data.

## Stability

Intensional stability is defined as the probability that the intent of a
concept is preserved when a random subset of its extent is removed.

$$\sigma(C) = \frac{\left| \{ A \subseteq \text{Ext}(C) \mid A\prime = \text{Int}(C)\} \right|}{2^{|\text{Ext}{(C)}|}}$$

Let’s compute it for the `planets` dataset:

``` r
fc <- FormalContext$new(planets)
fc$find_concepts()

stab <- fc$concepts$stability()
head(stab)
#> [1] 1.000000 0.703125 0.750000 0.750000 0.750000 0.562500
```

Concepts with stability close to 1 are very robust (likely real
patterns), while those near 0 are unstable (likely artifacts).

## Separation

Separation measures how many objects are “unique” to a concept $C$,
i.e., they are covered by $C$ but not by any of its direct subconcepts
(descendants).

$$\text{Sep}(C) = \left| \text{Ext}(C) \right| - \left| \bigcup\limits_{K \prec C}\text{Ext}(K) \right|$$

``` r
sep <- fc$concepts$separation()
#> Warning in Matrix.DeprecatedCoerce(cd1, cd2): 'as(<dgCMatrix>, "ngCMatrix")' is deprecated.
#> Use 'as(., "nMatrix")' instead.
#> See help("Deprecated") and help("Matrix-deprecated").
head(sep)
#> [1] 1 0 2 2 2 0
```

A high separation indicates that the concept introduces a significant
set of new objects into the hierarchy.

## Fuzzy density

In Fuzzy FCA, concepts are “rectangles” of high values in the matrix,
but not necessarily all 1s. Density measures the average value of the
relation $I$ within the concept.

$$\rho(C) = \frac{\sum\limits_{g \in \text{Ext}{(C)},m \in \text{Int}{(C)}}I(g,m)}{\left| \text{Ext}(C) \right| \cdot \left| \text{Int}(C) \right|}$$

``` r
# For binary data, density is always 1 (or 0 for empty concepts)
# We need to pass the original matrix I
dens <- fc$concepts$density(I = fc$incidence())
head(dens)
#> [1] 0 0 1 1 1 1
```

## Putting it all together

We can combine these metrics to filter and analyze the lattice.

``` r
df <- data.frame(
  Concept = 1:fc$concepts$size(),
  Support = fc$concepts$support(),
  Stability = stab,
  Separation = sep,
  Density = dens
)

# Concepts with high stability and separation
subset(df, Stability > 0.8 & Separation > 0)
#>   Concept Support Stability Separation Density
#> 1       1       0         1          1       0
```
