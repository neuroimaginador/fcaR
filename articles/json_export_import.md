# JSON Export and Import

In version 1.4.1, `fcaR` introduces comprehensive support for **JSON**
(JavaScript Object Notation) export and import. This facilitates
interoperability with other systems, web applications, and non-R based
tools.

This vignette demonstrates how to export and import the main data
structures in `fcaR`: `FormalContext`, `ConceptLattice`,
`ImplicationSet`, and `RuleSet`.

## Setup

First, ensure you have the `jsonlite` package installed, as it is
required for these features.

``` r
library(fcaR)
# install.packages("jsonlite")
```

## FormalContext

We can export a `FormalContext` to JSON, which preserves the objects,
attributes, and the incidence matrix. Importantly, the incidence matrix
is exported in a sparse format (indices and values) to handle large
datasets efficiently.

### Creating and Exporting

Let’s load a sample dataset and create a `FormalContext`.

``` r
data("planets")
fc <- FormalContext$new(planets)
print(fc)
#> FormalContext with 9 objects and 7 attributes.
#>          small  medium  large  near  far  moon  no_moon  
#>  Mercury   X                     X                 X     
#>    Venus   X                     X                 X     
#>    Earth   X                     X          X            
#>     Mars   X                     X          X            
#>  Jupiter                  X           X     X            
#>   Saturn                  X           X     X            
#>   Uranus           X                  X     X            
#>  Neptune           X                  X     X            
#>    Pluto   X                          X     X
```

To export the context to a JSON string:

``` r
json_str <- fc$to_json()
cat(substr(json_str, 1, 200), "...") # Print first 200 chars
#> {"type":"FormalContext","objects":["Mercury","Venus","Earth","Mars","Jupiter","Saturn","Uranus","Neptune","Pluto"],"attributes":["small","medium","large","near","far","moon","no_moon"],"I":{"indices": ...
```

You can also save it directly to a file:

``` r
fc$to_json(file = "context.json")
```

### Importing

To verify the export, we can import the JSON string back into a new
`FormalContext` object using
[`context_from_json()`](https://neuroimaginador.github.io/fcaR/reference/context_from_json.md):

``` r
fc2 <- context_from_json(json_str)
print(fc2)
#> FormalContext with 9 objects and 7 attributes.
#>          small  medium  large  near  far  moon  no_moon  
#>  Mercury   X                     X                 X     
#>    Venus   X                     X                 X     
#>    Earth   X                     X          X            
#>     Mars   X                     X          X            
#>  Jupiter                  X           X     X            
#>   Saturn                  X           X     X            
#>   Uranus           X                  X     X            
#>  Neptune           X                  X     X            
#>    Pluto   X                          X     X
```

We can check that the original and reconstructed contexts are identical:

``` r
all(fc$objects == fc2$objects)
#> [1] TRUE
all(fc$attributes == fc2$attributes)
#> [1] TRUE
all(as.matrix(fc$I) == as.matrix(fc2$I))
#> [1] TRUE
```

### Recursive Export

The `to_json()` method for `FormalContext` is **recursive**. If you have
computed concepts or implications, they will be nested within the
exported JSON.

``` r
fc$find_concepts()
fc$find_implications()

# Export with nested concepts and implications
json_full <- fc$to_json()
#> 'as(<ngCMatrix>, "dgCMatrix")' is deprecated.
#> Use 'as(., "dMatrix")' instead.
#> See help("Deprecated") and help("Matrix-deprecated").

# Import back
fc_full <- context_from_json(json_full)

# Check if concepts are present
fc_full$concepts$size()
#> [1] 12
```

## ConceptLattice

You can also export a `ConceptLattice` independently.

``` r
cl <- fc$concepts
json_lattice <- cl$to_json()
```

And import it using
[`lattice_from_json()`](https://neuroimaginador.github.io/fcaR/reference/lattice_from_json.md):

``` r
cl2 <- lattice_from_json(json_lattice)
print(cl2)
#> A set of 12 concepts:
#> 1: ({Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto}, {})
#> 2: ({Earth, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto}, {moon})
#> 3: ({Jupiter, Saturn, Uranus, Neptune, Pluto}, {far, moon})
#> 4: ({Jupiter, Saturn}, {large, far, moon})
#> 5: ({Uranus, Neptune}, {medium, far, moon})
#> 6: ({Mercury, Venus, Earth, Mars, Pluto}, {small})
#> 7: ({Earth, Mars, Pluto}, {small, moon})
#> 8: ({Pluto}, {small, far, moon})
#> 9: ({Mercury, Venus, Earth, Mars}, {small, near})
#> 10: ({Mercury, Venus}, {small, near, no_moon})
#> 11: ({Earth, Mars}, {small, near, moon})
#> 12: ({}, {small, medium, large, near, far, moon, no_moon})
```

The exported JSON includes the lattice hierarchy
(superconcept/subconcept relations), allowing for full reconstruction of
the lattice structure.

## ImplicationSet

Similarly, sets of implications can be exported and imported.

``` r
imps <- fc$implications
json_imps <- imps$to_json()
```

Import using
[`implications_from_json()`](https://neuroimaginador.github.io/fcaR/reference/implications_from_json.md):

``` r
imps2 <- implications_from_json(json_imps)
print(imps2)
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

## RuleSet

Association rules (including causal rules) are also supported.

``` r
# Assuming we have a RuleSet, e.g. from arules or created manually
# Here we'll just demonstrate the syntax
rs <- RuleSet$new(attributes = fc$attributes)
# ... populate rules ...
# json_rules <- rs$to_json()
# rs2 <- rules_from_json(json_rules)
```

## Summary

The new JSON functionality ensures that you can easily move your FCA
models out of R for visualization, storage, or integration with web
services.

| Class            | Export Method | Import Function                                                                                          |
|------------------|---------------|----------------------------------------------------------------------------------------------------------|
| `FormalContext`  | `$to_json()`  | [`context_from_json()`](https://neuroimaginador.github.io/fcaR/reference/context_from_json.md)           |
| `ConceptLattice` | `$to_json()`  | [`lattice_from_json()`](https://neuroimaginador.github.io/fcaR/reference/lattice_from_json.md)           |
| `ImplicationSet` | `$to_json()`  | [`implications_from_json()`](https://neuroimaginador.github.io/fcaR/reference/implications_from_json.md) |
| `RuleSet`        | `$to_json()`  | [`rules_from_json()`](https://neuroimaginador.github.io/fcaR/reference/rules_from_json.md)               |
