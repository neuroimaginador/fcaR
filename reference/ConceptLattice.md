# R6 class for a concept lattice

This class implements the data structure and methods for concept
lattices.

## Super class

[`fcaR::ConceptSet`](https://neuroimaginador.github.io/fcaR/reference/ConceptSet.md)
-\> `ConceptLattice`

## Methods

### Public methods

- [`ConceptLattice$new()`](#method-ConceptLattice-new)

- [`ConceptLattice$plot()`](#method-ConceptLattice-plot)

- [`ConceptLattice$sublattice()`](#method-ConceptLattice-sublattice)

- [`ConceptLattice$top()`](#method-ConceptLattice-top)

- [`ConceptLattice$bottom()`](#method-ConceptLattice-bottom)

- [`ConceptLattice$join_irreducibles()`](#method-ConceptLattice-join_irreducibles)

- [`ConceptLattice$meet_irreducibles()`](#method-ConceptLattice-meet_irreducibles)

- [`ConceptLattice$decompose()`](#method-ConceptLattice-decompose)

- [`ConceptLattice$supremum()`](#method-ConceptLattice-supremum)

- [`ConceptLattice$infimum()`](#method-ConceptLattice-infimum)

- [`ConceptLattice$subconcepts()`](#method-ConceptLattice-subconcepts)

- [`ConceptLattice$superconcepts()`](#method-ConceptLattice-superconcepts)

- [`ConceptLattice$lower_neighbours()`](#method-ConceptLattice-lower_neighbours)

- [`ConceptLattice$upper_neighbours()`](#method-ConceptLattice-upper_neighbours)

- [`ConceptLattice$stability()`](#method-ConceptLattice-stability)

- [`ConceptLattice$separation()`](#method-ConceptLattice-separation)

- [`ConceptLattice$density()`](#method-ConceptLattice-density)

- [`ConceptLattice$is_distributive()`](#method-ConceptLattice-is_distributive)

- [`ConceptLattice$is_modular()`](#method-ConceptLattice-is_modular)

- [`ConceptLattice$is_semimodular()`](#method-ConceptLattice-is_semimodular)

- [`ConceptLattice$is_atomic()`](#method-ConceptLattice-is_atomic)

- [`ConceptLattice$clone()`](#method-ConceptLattice-clone)

Inherited methods

- [`fcaR::ConceptSet$[()`](https://neuroimaginador.github.io/fcaR/reference/ConceptSet.html#method-%5B)
- [`fcaR::ConceptSet$extents()`](https://neuroimaginador.github.io/fcaR/reference/ConceptSet.html#method-extents)
- [`fcaR::ConceptSet$intents()`](https://neuroimaginador.github.io/fcaR/reference/ConceptSet.html#method-intents)
- [`fcaR::ConceptSet$is_empty()`](https://neuroimaginador.github.io/fcaR/reference/ConceptSet.html#method-is_empty)
- [`fcaR::ConceptSet$print()`](https://neuroimaginador.github.io/fcaR/reference/ConceptSet.html#method-print)
- [`fcaR::ConceptSet$size()`](https://neuroimaginador.github.io/fcaR/reference/ConceptSet.html#method-size)
- [`fcaR::ConceptSet$sub()`](https://neuroimaginador.github.io/fcaR/reference/ConceptSet.html#method-sub)
- [`fcaR::ConceptSet$support()`](https://neuroimaginador.github.io/fcaR/reference/ConceptSet.html#method-support)
- [`fcaR::ConceptSet$to_latex()`](https://neuroimaginador.github.io/fcaR/reference/ConceptSet.html#method-to_latex)
- [`fcaR::ConceptSet$to_list()`](https://neuroimaginador.github.io/fcaR/reference/ConceptSet.html#method-to_list)

------------------------------------------------------------------------

### Method `new()`

Create a new `ConceptLattice` object.

#### Usage

    ConceptLattice$new(extents, intents, objects, attributes, I = NULL)

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

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Plot the concept lattice

#### Usage

    ConceptLattice$plot(
      object_names = TRUE,
      to_latex = FALSE,
      method = c("sugiyama", "force"),
      mode = NULL,
      ...
    )

#### Arguments

- `object_names`:

  (logical) Deprecated. Use `mode` instead. If `TRUE` (default), implies
  `mode = "reduced"` or similar depending on heuristics. Kept for
  backward compatibility.

- `to_latex`:

  (logical) If `TRUE`, exports the plot as TikZ code (LaTeX) instead of
  drawing it. Returns an object of class `tikz_code` that prints the
  LaTeX code to console.

- `method`:

  (character) The layout algorithm to use. Options are:

  - `"sugiyama"` (default): A hierarchical layout that minimizes edge
    crossings and centers nodes (similar to ConExp or hasseDiagram).

  - `"force"`: A force-directed (spring) layout, useful for large or
    non-hierarchical lattices.

- `mode`:

  (character) The labeling mode for the nodes. If `NULL` (default), a
  heuristic based on lattice size is used. Options are:

  - `"reduced"`: Standard FCA labeling. Nodes are labeled with an
    attribute (or object) only if they are the supreme (or infimum) of
    that attribute (or object).

  - `"full"`: Each node shows its complete extent and intent.

  - `"attributes"`: Nodes show only their intent (attributes).

  - `"empty"`: Nodes are drawn as points without labels. Recommended for
    very large lattices (\>50 concepts).

- `...`:

  Other parameters passed to the internal plotting function (e.g.,
  graphical parameters for `ggraph`).

#### Returns

If `to_latex` is `FALSE`, it returns (invisibly) the `ggplot2` object
representing the graph. If `to_latex` is `TRUE`, it returns a
`tikz_code` object containing the LaTeX code.

------------------------------------------------------------------------

### Method `sublattice()`

Sublattice

#### Usage

    ConceptLattice$sublattice(...)

#### Arguments

- `...`:

  See Details.

#### Details

As argument, one can provide both integer indices or `Concepts`,
separated by commas. The corresponding concepts are used to generate a
sublattice.

#### Returns

The generated sublattice as a new `ConceptLattice` object.

------------------------------------------------------------------------

### Method `top()`

Top of a Lattice

#### Usage

    ConceptLattice$top()

#### Returns

The top of the Concept Lattice

#### Examples

    fc <- FormalContext$new(planets)
    fc$find_concepts()
    fc$concepts$top()

------------------------------------------------------------------------

### Method `bottom()`

Bottom of a Lattice

#### Usage

    ConceptLattice$bottom()

#### Returns

The bottom of the Concept Lattice

#### Examples

    fc <- FormalContext$new(planets)
    fc$find_concepts()
    fc$concepts$bottom()

------------------------------------------------------------------------

### Method `join_irreducibles()`

Join-irreducible Elements

#### Usage

    ConceptLattice$join_irreducibles()

#### Returns

The join-irreducible elements in the concept lattice.

------------------------------------------------------------------------

### Method `meet_irreducibles()`

Meet-irreducible Elements

#### Usage

    ConceptLattice$meet_irreducibles()

#### Returns

The meet-irreducible elements in the concept lattice.

------------------------------------------------------------------------

### Method [`decompose()`](https://rdrr.io/r/stats/decompose.html)

Decompose a concept as the supremum of meet-irreducible concepts

#### Usage

    ConceptLattice$decompose(C)

#### Arguments

- `C`:

  A list of `Concept`s

#### Returns

A list, each field is the set of meet-irreducible elements whose
supremum is the corresponding element in `C`.

------------------------------------------------------------------------

### Method `supremum()`

Supremum of Concepts

#### Usage

    ConceptLattice$supremum(...)

#### Arguments

- `...`:

  See Details.

#### Details

As argument, one can provide both integer indices or `Concepts`,
separated by commas. The corresponding concepts are used to compute
their supremum in the lattice.

#### Returns

The supremum of the list of concepts.

------------------------------------------------------------------------

### Method `infimum()`

Infimum of Concepts

#### Usage

    ConceptLattice$infimum(...)

#### Arguments

- `...`:

  See Details.

#### Details

As argument, one can provide both integer indices or `Concepts`,
separated by commas. The corresponding concepts are used to compute
their infimum in the lattice.

#### Returns

The infimum of the list of concepts.

------------------------------------------------------------------------

### Method `subconcepts()`

Subconcepts of a Concept

#### Usage

    ConceptLattice$subconcepts(C)

#### Arguments

- `C`:

  (numeric or `SparseConcept`) The concept to which determine all its
  subconcepts.

#### Returns

A list with the subconcepts.

------------------------------------------------------------------------

### Method `superconcepts()`

Superconcepts of a Concept

#### Usage

    ConceptLattice$superconcepts(C)

#### Arguments

- `C`:

  (numeric or `SparseConcept`) The concept to which determine all its
  superconcepts.

#### Returns

A list with the superconcepts.

------------------------------------------------------------------------

### Method `lower_neighbours()`

Lower Neighbours of a Concept

#### Usage

    ConceptLattice$lower_neighbours(C)

#### Arguments

- `C`:

  (`SparseConcept`) The concept to which find its lower neighbours

#### Returns

A list with the lower neighbours of `C`.

------------------------------------------------------------------------

### Method `upper_neighbours()`

Upper Neighbours of a Concept

#### Usage

    ConceptLattice$upper_neighbours(C)

#### Arguments

- `C`:

  (`SparseConcept`) The concept to which find its upper neighbours

#### Returns

A list with the upper neighbours of `C`.

------------------------------------------------------------------------

### Method `stability()`

Computes the stability of each concept.

#### Usage

    ConceptLattice$stability()

#### Returns

A numeric vector with the stability of each concept.

------------------------------------------------------------------------

### Method `separation()`

Computes the separation of each concept. Separation is the number of
objects covered by the concept but not by any of its immediate
subconcepts.

#### Usage

    ConceptLattice$separation()

#### Returns

A numeric vector with the separation of each concept.

------------------------------------------------------------------------

### Method [`density()`](https://rdrr.io/r/stats/density.html)

Computes the fuzzy density of each concept.

#### Usage

    ConceptLattice$density(I = NULL)

#### Arguments

- `I`:

  (Optional) The original incidence matrix. If NULL, it tries to access
  it from the parent FormalContext if linked.

#### Returns

A numeric vector with the density of each concept.

------------------------------------------------------------------------

### Method `is_distributive()`

Check if the lattice is distributive. A lattice is distributive if \\x
\wedge (y \vee z) = (x \wedge y) \vee (x \wedge z)\\ for all elements.

#### Usage

    ConceptLattice$is_distributive()

#### Returns

Logical.

------------------------------------------------------------------------

### Method `is_modular()`

Check if the lattice is modular. A lattice is modular if \\x \le z
\implies x \vee (y \wedge z) = (x \vee y) \wedge z\\. Distributive
lattices are always modular.

#### Usage

    ConceptLattice$is_modular()

#### Returns

Logical.

------------------------------------------------------------------------

### Method `is_semimodular()`

Check if the lattice is upper semimodular. A lattice is upper
semimodular if for every \\x, y\\: if \\x\\ covers \\x \wedge y\\, then
\\x \vee y\\ covers \\y\\.

#### Usage

    ConceptLattice$is_semimodular()

#### Returns

Logical.

------------------------------------------------------------------------

### Method `is_atomic()`

Check if the lattice is atomic. A lattice is atomic if for every element
\\x \> \bot\\, there exists an atom \\a\\ such that \\a \le x\\. Atoms
are elements that cover the bottom element.

#### Usage

    ConceptLattice$is_atomic()

#### Returns

Logical.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ConceptLattice$clone(deep = FALSE)

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

# Get concept support
fc_planets$concepts$support()
#>  [1] 0.0000000 1.0000000 0.2222222 0.2222222 0.2222222 0.4444444 0.2222222
#>  [8] 0.5555556 0.1111111 0.3333333 0.5555556 0.7777778


## ------------------------------------------------
## Method `ConceptLattice$top`
## ------------------------------------------------

fc <- FormalContext$new(planets)
fc$find_concepts()
fc$concepts$top()
#> ({Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto}, {})


## ------------------------------------------------
## Method `ConceptLattice$bottom`
## ------------------------------------------------

fc <- FormalContext$new(planets)
fc$find_concepts()
fc$concepts$bottom()
#> ({}, {small, medium, large, near, far, moon, no_moon})
```
