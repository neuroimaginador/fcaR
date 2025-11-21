# R6 class for a fuzzy concept with sparse internal representation

This class implements the data structure and methods for fuzzy concepts.

## Methods

### Public methods

- [`Concept$new()`](#method-Concept-new)

- [`Concept$get_extent()`](#method-Concept-get_extent)

- [`Concept$get_intent()`](#method-Concept-get_intent)

- [`Concept$print()`](#method-Concept-print)

- [`Concept$to_latex()`](#method-Concept-to_latex)

- [`Concept$clone()`](#method-Concept-clone)

------------------------------------------------------------------------

### Method `new()`

Creator for objects of class `Concept`

#### Usage

    Concept$new(extent, intent)

#### Arguments

- `extent`:

  (`Set`) The extent of the concept.

- `intent`:

  (`Set`) The intent of the concept.

#### Returns

An object of class `Concept`.

------------------------------------------------------------------------

### Method `get_extent()`

Internal `Set` for the extent

#### Usage

    Concept$get_extent()

#### Returns

The `Set` representation of the extent.

------------------------------------------------------------------------

### Method `get_intent()`

Internal `Set` for the intent

#### Usage

    Concept$get_intent()

#### Returns

The `Set` representation of the intent.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints the concept to console

#### Usage

    Concept$print()

#### Returns

A string with the elements of the set and their grades between brackets
.

------------------------------------------------------------------------

### Method `to_latex()`

Write the concept in LaTeX format

#### Usage

    Concept$to_latex(print = TRUE)

#### Arguments

- `print`:

  (logical) Print to output?

#### Returns

The fuzzy concept in LaTeX.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Concept$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Build a formal context and find its concepts
fc_planets <- FormalContext$new(planets)
fc_planets$find_concepts()
#> [DEBUG] Entered InClose_Reorder
#> [DEBUG] Extracting Data...
#> [DEBUG] Data Extracted. Objects: 9 Attributes: 7
#> [DEBUG] Sorting attributes...
#> [DEBUG] Building Bitsets...
#> [DEBUG] Initializing Context...
#> [DEBUG] Starting Recursion...
#> [DEBUG] Recursion Depth: 0 Y: -1
#> [DEBUG] Recursion Depth: 1 Y: 0
#> [DEBUG] Recursion Depth: 1 Y: 1
#> [DEBUG] Recursion Depth: 1 Y: 2
#> [DEBUG] Recursion Depth: 1 Y: 3
#> [DEBUG] Recursion Depth: 1 Y: 4
#> [DEBUG] Recursion Depth: 1 Y: 5
#> [DEBUG] Recursion Depth: 1 Y: 6
#> [DEBUG] Recursion Finished.
#> [DEBUG] Packaging Results: 12 concepts.
#> [DEBUG] Done.

# Print the first three concepts
fc_planets$concepts[1:3]
#> A set of 3 concepts:
#> 1: ({}, {small, medium, large, near, far, moon, no_moon})
#> 2: ({Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto}, {})
#> 3: ({Uranus, Neptune}, {medium, far, moon})

# Select the first concept:
C <- fc_planets$concepts$sub(1)

# Get its extent and intent
C$get_extent()
#> {}
C$get_intent()
#> {small, medium, large, near, far, moon, no_moon}
```
