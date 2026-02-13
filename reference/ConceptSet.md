# R6 class for a set of concepts

This class implements the data structure and methods for concept sets.

## Methods

### Public methods

- [`ConceptSet$new()`](#method-ConceptSet-new)

- [`ConceptSet$size()`](#method-ConceptSet-size)

- [`ConceptSet$is_empty()`](#method-ConceptSet-is_empty)

- [`ConceptSet$extents()`](#method-ConceptSet-extents)

- [`ConceptSet$intents()`](#method-ConceptSet-intents)

- [`ConceptSet$print()`](#method-ConceptSet-print)

- [`ConceptSet$to_latex()`](#method-ConceptSet-to_latex)

- [`ConceptSet$to_list()`](#method-ConceptSet-to_list)

- [`ConceptSet$[()`](#method-ConceptSet-%5B)

- [`ConceptSet$sub()`](#method-ConceptSet-sub)

- [`ConceptSet$support()`](#method-ConceptSet-support)

- [`ConceptSet$stability()`](#method-ConceptSet-stability)

- [`ConceptSet$clone()`](#method-ConceptSet-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `ConceptLattice` object.

#### Usage

    ConceptSet$new(extents, intents, objects, attributes, I = NULL)

#### Arguments

- `extents`:

  (`dgCMatrix`) The extents of all concepts

- `intents`:

  (`dgCMatrix`) The intents of all concepts

- `objects`:

  (character vector) Names of the objects in the formal context

- `attributes`:

  (character vector) Names of the attributes in the formal context

- `I`:

  (`dgCMatrix`) The matrix of the formal context

#### Returns

A new `ConceptLattice` object.

------------------------------------------------------------------------

### Method `size()`

Size of the Lattice

#### Usage

    ConceptSet$size()

#### Returns

The number of concepts in the lattice.

------------------------------------------------------------------------

### Method `is_empty()`

Is the lattice empty?

#### Usage

    ConceptSet$is_empty()

#### Returns

`TRUE` if the lattice has no concepts.

------------------------------------------------------------------------

### Method `extents()`

Concept Extents

#### Usage

    ConceptSet$extents()

#### Returns

The extents of all concepts, as a `dgCMatrix`.

------------------------------------------------------------------------

### Method `intents()`

Concept Intents

#### Usage

    ConceptSet$intents()

#### Returns

The intents of all concepts, as a `dgCMatrix`.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the Concept Set

#### Usage

    ConceptSet$print()

#### Returns

Nothing, just prints the concepts

------------------------------------------------------------------------

### Method `to_latex()`

Write in LaTeX

#### Usage

    ConceptSet$to_latex(print = TRUE, ncols = 1, numbered = TRUE, align = TRUE)

#### Arguments

- `print`:

  (logical) Print to output?

- `ncols`:

  (integer) Number of columns of the output.

- `numbered`:

  (logical) Number the concepts?

- `align`:

  (logical) Align objects and attributes independently?

#### Returns

The `LaTeX` code to list all concepts.

------------------------------------------------------------------------

### Method `to_list()`

Returns a list with all the concepts

#### Usage

    ConceptSet$to_list()

#### Returns

A list of concepts.

------------------------------------------------------------------------

### Method `[()`

Subsets a ConceptSet

#### Usage

    ConceptSet$[(indices)

#### Arguments

- `indices`:

  (numeric or logical vector) The indices of the concepts to return as a
  list of Concepts. It can be a vector of logicals where `TRUE` elements
  are to be retained.

#### Returns

Another ConceptSet.

------------------------------------------------------------------------

### Method [`sub()`](https://rdrr.io/r/base/grep.html)

Individual Concepts

#### Usage

    ConceptSet$sub(index)

#### Arguments

- `index`:

  (numeric) The index of the concept to return.

#### Returns

The Concept.

------------------------------------------------------------------------

### Method `support()`

Get support of each concept

#### Usage

    ConceptSet$support()

#### Returns

A vector with the support of each concept.

------------------------------------------------------------------------

### Method `stability()`

Compute the stability of each concept

#### Usage

    ConceptSet$stability()

#### Returns

A numeric vector with the stability of each concept.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ConceptSet$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Build a formal context
fc_planets <- FormalContext$new(planets)

# Find the concepts
fc_planets$find_concepts()

# Find join- and meet- irreducible elements
fc_planets$concepts$join_irreducibles()
#> A set of 5 concepts:
#> 1: ({Uranus, Neptune}, {medium, far, moon})
#> 2: ({Jupiter, Saturn}, {large, far, moon})
#> 3: ({Mercury, Venus}, {small, near, no_moon})
#> 4: ({Earth, Mars}, {small, near, moon})
#> 5: ({Pluto}, {small, far, moon})
fc_planets$concepts$meet_irreducibles()
#> A set of 7 concepts:
#> 1: ({Uranus, Neptune}, {medium, far, moon})
#> 2: ({Jupiter, Saturn}, {large, far, moon})
#> 3: ({Mercury, Venus}, {small, near, no_moon})
#> 4: ({Mercury, Venus, Earth, Mars}, {small, near})
#> 5: ({Mercury, Venus, Earth, Mars, Pluto}, {small})
#> 6: ({Jupiter, Saturn, Uranus, Neptune, Pluto}, {far, moon})
#> 7: ({Earth, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto}, {moon})
```
