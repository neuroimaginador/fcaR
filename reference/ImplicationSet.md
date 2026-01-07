# R6 Class for Set of implications

This class implements the structure needed to store implications and the
methods associated.

## References

Ganter B, Obiedkov S (2016). Conceptual Exploration. Springer.
https://doi.org/10.1007/978-3-662-49291-8

Hahsler M, Grun B, Hornik K (2005). “arules - a computational
environment for mining association rules and frequent item sets.” *J
Stat Softw*, *14*, 1-25.

Belohlavek R, Cordero P, Enciso M, Mora Á, Vychodil V (2016). “Automated
prover for attribute dependencies in data with grades.” *International
Journal of Approximate Reasoning*, *70*, 51-67.

Mora A, Cordero P, Enciso M, Fortes I, Aguilera G (2012). “Closure via
functional dependence simplification.” *International Journal of
Computer Mathematics*, *89*(4), 510-526.

## Methods

### Public methods

- [`ImplicationSet$new()`](#method-ImplicationSet-new)

- [`ImplicationSet$get_attributes()`](#method-ImplicationSet-get_attributes)

- [`ImplicationSet$[()`](#method-ImplicationSet-bracket)

- [`ImplicationSet$to_arules()`](#method-ImplicationSet-to_arules)

- [`ImplicationSet$add()`](#method-ImplicationSet-add)

- [`ImplicationSet$cardinality()`](#method-ImplicationSet-cardinality)

- [`ImplicationSet$is_empty()`](#method-ImplicationSet-is_empty)

- [`ImplicationSet$size()`](#method-ImplicationSet-size)

- [`ImplicationSet$closure()`](#method-ImplicationSet-closure)

- [`ImplicationSet$recommend()`](#method-ImplicationSet-recommend)

- [`ImplicationSet$apply_rules()`](#method-ImplicationSet-apply_rules)

- [`ImplicationSet$to_basis()`](#method-ImplicationSet-to_basis)

- [`ImplicationSet$to_direct_optimal()`](#method-ImplicationSet-to_direct_optimal)

- [`ImplicationSet$print()`](#method-ImplicationSet-print)

- [`ImplicationSet$to_latex()`](#method-ImplicationSet-to_latex)

- [`ImplicationSet$get_LHS_matrix()`](#method-ImplicationSet-get_LHS_matrix)

- [`ImplicationSet$get_RHS_matrix()`](#method-ImplicationSet-get_RHS_matrix)

- [`ImplicationSet$filter()`](#method-ImplicationSet-filter)

- [`ImplicationSet$support()`](#method-ImplicationSet-support)

- [`ImplicationSet$use_logic()`](#method-ImplicationSet-use_logic)

- [`ImplicationSet$get_logic()`](#method-ImplicationSet-get_logic)

- [`ImplicationSet$use_hedge()`](#method-ImplicationSet-use_hedge)

- [`ImplicationSet$get_hedge()`](#method-ImplicationSet-get_hedge)

- [`ImplicationSet$clone()`](#method-ImplicationSet-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize with an optional name

#### Usage

    ImplicationSet$new(...)

#### Arguments

- `...`:

  See Details.

#### Details

Creates and initialize a new `ImplicationSet` object. It can be done in
two ways: `initialize(name, attributes, lhs, rhs)` or
`initialize(rules)`

In the first way, the only mandatory argument is `attributes`,
(character vector) which is a vector of names of the attributes on which
we define the implications. Optional arguments are: `name` (character
string), name of the implication set, `lhs` (a `dgCMatrix`), initial LHS
of the implications stored and the analogous `rhs`.

The other way is used to initialize the `ImplicationSet` object from a
`rules` object from package `arules`.

#### Returns

A new `ImplicationSet` object.

------------------------------------------------------------------------

### Method `get_attributes()`

Get the names of the attributes

#### Usage

    ImplicationSet$get_attributes()

#### Returns

A character vector with the names of the attributes used in the
implications.

------------------------------------------------------------------------

### Method `[()`

Get a subset of the implication set

#### Usage

    ImplicationSet$[(idx)

#### Arguments

- `idx`:

  (integer or logical vector) Indices of the implications to extract or
  remove. If logical vector, only `TRUE` elements are retained and the
  rest discarded.

#### Returns

A new `ImplicationSet` with only the rules given by the `idx` indices
(if all `idx > 0` and all but `idx` if all `idx < 0`.

------------------------------------------------------------------------

### Method `to_arules()`

Convert to arules format

#### Usage

    ImplicationSet$to_arules(quality = TRUE)

#### Arguments

- `quality`:

  (logical) Compute the interest measures for each rule?

#### Returns

A `rules` object as used by package `arules`.

------------------------------------------------------------------------

### Method `add()`

Add a precomputed implication set

#### Usage

    ImplicationSet$add(...)

#### Arguments

- `...`:

  An `ImplicationSet` object, a `rules` object, or a pair `lhs`, `rhs`
  of `Set` objects or `dgCMatrix`. The implications to add to this
  formal context.

#### Returns

Nothing, just updates the internal `implications` field.

------------------------------------------------------------------------

### Method `cardinality()`

Cardinality: Number of implications in the set

#### Usage

    ImplicationSet$cardinality()

#### Returns

The cardinality of the implication set.

------------------------------------------------------------------------

### Method `is_empty()`

Empty set

#### Usage

    ImplicationSet$is_empty()

#### Returns

`TRUE` if the set of implications is empty, `FALSE` otherwise.

------------------------------------------------------------------------

### Method `size()`

Size: number of attributes in each of LHS and RHS

#### Usage

    ImplicationSet$size()

#### Returns

A vector with two components: the number of attributes present in each
of the LHS and RHS of each implication in the set.

------------------------------------------------------------------------

### Method `closure()`

Compute the semantic closure of a fuzzy set with respect to the
implication set

#### Usage

    ImplicationSet$closure(S, reduce = FALSE, verbose = FALSE)

#### Arguments

- `S`:

  (a `Set` object) Fuzzy set to compute its closure. Use class `Set` to
  build it.

- `reduce`:

  (logical) Reduce the implications using simplification logic?

- `verbose`:

  (logical) Show verbose output?

#### Returns

If `reduce == FALSE`, the output is a fuzzy set corresponding to the
closure of `S`. If `reduce == TRUE`, a list with two components:
`closure`, with the closure as above, and `implications`, the reduced
set of implications.

------------------------------------------------------------------------

### Method `recommend()`

Generate a recommendation for a subset of the attributes

#### Usage

    ImplicationSet$recommend(S, attribute_filter)

#### Arguments

- `S`:

  (a vector) Vector with the grades of each attribute (a fuzzy set).

- `attribute_filter`:

  (character vector) Names of the attributes to get recommendation for.

#### Returns

A fuzzy set describing the values of the attributes in
`attribute_filter` within the closure of `S`.

------------------------------------------------------------------------

### Method `apply_rules()`

Apply rules to remove redundancies

#### Usage

    ImplicationSet$apply_rules(
      rules = c("composition", "generalization"),
      batch_size = 25000L,
      parallelize = FALSE,
      reorder = FALSE
    )

#### Arguments

- `rules`:

  (character vector) Names of the rules to use. See `details`.

- `batch_size`:

  (integer) If the number of rules is large, apply the rules by batches
  of this size.

- `parallelize`:

  (logical) If possible, should we parallelize the computation among
  different batches?

- `reorder`:

  (logical) Should the rules be randomly reordered previous to the
  computation?

#### Details

Currently, the implemented rules are `"generalization"`,
`"simplification"`, `"reduction"` and `"composition"`.

#### Returns

Nothing, just updates the internal matrices for LHS and RHS.

------------------------------------------------------------------------

### Method `to_basis()`

Convert Implications to Canonical Basis

#### Usage

    ImplicationSet$to_basis()

#### Returns

The canonical basis of implications obtained from the current
`ImplicationSet`

------------------------------------------------------------------------

### Method `to_direct_optimal()`

Compute the Direct Optimal Basis using optimized C++ algorithms.

#### Usage

    ImplicationSet$to_direct_optimal(
      method = c("direct_optimal", "final_ts", "monotonic", "priority"),
      verbose = FALSE
    )

#### Arguments

- `method`:

  (character) The specific algorithm to run:

  - `"direct_optimal"`: (Default) The Direct Optimal Saturation-Pruning
    algorithm.

  - `"final_ts"`: Computes Transitive Closure then Prunes (Standard
    approach).

  - `"monotonic"`: Incremental algorithm maintaining monotonicity.

  - `"priority"`: Priority-based refinement algorithm.

- `verbose`:

  (logical) Print verbose output from the C++ backend.

#### Returns

Nothing, updates the `ImplicationSet` in place with the new basis.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print all implications to text

#### Usage

    ImplicationSet$print()

#### Returns

A string with all the implications in the set.

------------------------------------------------------------------------

### Method `to_latex()`

Export to LaTeX

#### Usage

    ImplicationSet$to_latex(
      print = TRUE,
      ncols = 1,
      numbered = TRUE,
      numbers = seq(self$cardinality())
    )

#### Arguments

- `print`:

  (logical) Print to output?

- `ncols`:

  (integer) Number of columns for the output.

- `numbered`:

  (logical) If `TRUE` (default), implications will be numbered in the
  output.

- `numbers`:

  (vector) If `numbered`, use these elements to enumerate the
  implications. The default is to enumerate 1, 2, ..., but can be
  changed.

#### Returns

A string in LaTeX format that prints nicely all the implications.

------------------------------------------------------------------------

### Method `get_LHS_matrix()`

Get internal LHS matrix

#### Usage

    ImplicationSet$get_LHS_matrix()

#### Returns

A sparse matrix representing the LHS of the implications in the set.

------------------------------------------------------------------------

### Method `get_RHS_matrix()`

Get internal RHS matrix

#### Usage

    ImplicationSet$get_RHS_matrix()

#### Returns

A sparse matrix representing the RHS of the implications in the set.

------------------------------------------------------------------------

### Method [`filter()`](https://rdrr.io/r/stats/filter.html)

Filter implications by attributes in LHS and RHS

#### Usage

    ImplicationSet$filter(
      lhs = NULL,
      not_lhs = NULL,
      rhs = NULL,
      not_rhs = NULL,
      drop = FALSE
    )

#### Arguments

- `lhs`:

  (character vector) Names of the attributes to filter the LHS by. If
  `NULL`, no filtering is done on the LHS.

- `not_lhs`:

  (character vector) Names of the attributes to not include in the LHS.
  If `NULL` (the default), it is not considered at all.

- `rhs`:

  (character vector) Names of the attributes to filter the RHS by. If
  `NULL`, no filtering is done on the RHS.

- `not_rhs`:

  (character vector) Names of the attributes to not include in the RHS.
  If `NULL` (the default), it is not considered at all.

- `drop`:

  (logical) Remove the rest of attributes in RHS?

#### Returns

An `ImplicationSet` that is a subset of the current set, only with those
rules which has the attributes in `lhs` and `rhs` in their LHS and RHS,
respectively.

------------------------------------------------------------------------

### Method `support()`

Compute support of each implication

#### Usage

    ImplicationSet$support()

#### Returns

A vector with the support of each implication

------------------------------------------------------------------------

### Method `use_logic()`

Sets the logic to use

#### Usage

    ImplicationSet$use_logic(name = available_logics())

#### Arguments

- `name`:

  The name of the logic to use. To see the available names, run
  `available_logics()`.

------------------------------------------------------------------------

### Method `get_logic()`

Gets the logic used

#### Usage

    ImplicationSet$get_logic()

#### Returns

A string with the name of the logic.

------------------------------------------------------------------------

### Method `use_hedge()`

Sets the hedge to use when computing closures

#### Usage

    ImplicationSet$use_hedge(name = c("globalization", "identity"))

#### Arguments

- `name`:

  The name of the hedge to use. Only "globalization" and "identity" are
  allowed.

------------------------------------------------------------------------

### Method `get_hedge()`

Gets the hedge used to compute closures

#### Usage

    ImplicationSet$get_hedge()

#### Returns

A string with the name of the hedge

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ImplicationSet$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Build a formal context
fc_planets <- FormalContext$new(planets)

# Find its implication basis
fc_planets$find_implications()

# Print implications
fc_planets$implications
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

# Cardinality and mean size in the ruleset
fc_planets$implications$cardinality()
#> [1] 10
sizes <- fc_planets$implications$size()
colMeans(sizes)
#> LHS RHS 
#> 2.5 2.3 

# Simplify the implication set
fc_planets$implications$apply_rules("simplification")
#> Processing batch
#> --> Simplification: from 10 to 10 in 0.037 secs.
#> Batch took 0.04 secs. 
```
