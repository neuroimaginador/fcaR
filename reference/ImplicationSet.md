# R6 class for an Implication Set

This class implements an implication set (LHS -\> RHS) in the framework
of Formal Concept Analysis (FCA). It inherits from `RuleSet` and adds
FCA-specific methods such as closure computation, simplification, and
basis transformation.

## Super class

[`fcaR::RuleSet`](https://neuroimaginador.github.io/fcaR/reference/RuleSet.md)
-\> `ImplicationSet`

## Methods

### Public methods

- [`ImplicationSet$new()`](#method-ImplicationSet-new)

- [`ImplicationSet$add()`](#method-ImplicationSet-add)

- [`ImplicationSet$to_arules()`](#method-ImplicationSet-to_arules)

- [`ImplicationSet$print()`](#method-ImplicationSet-print)

- [`ImplicationSet$support()`](#method-ImplicationSet-support)

- [`ImplicationSet$closure()`](#method-ImplicationSet-closure)

- [`ImplicationSet$recommend()`](#method-ImplicationSet-recommend)

- [`ImplicationSet$apply_rules()`](#method-ImplicationSet-apply_rules)

- [`ImplicationSet$to_basis()`](#method-ImplicationSet-to_basis)

- [`ImplicationSet$to_direct_optimal()`](#method-ImplicationSet-to_direct_optimal)

- [`ImplicationSet$use_logic()`](#method-ImplicationSet-use_logic)

- [`ImplicationSet$get_logic()`](#method-ImplicationSet-get_logic)

- [`ImplicationSet$use_hedge()`](#method-ImplicationSet-use_hedge)

- [`ImplicationSet$get_hedge()`](#method-ImplicationSet-get_hedge)

- [`ImplicationSet$to_json()`](#method-ImplicationSet-to_json)

- [`ImplicationSet$clone()`](#method-ImplicationSet-clone)

Inherited methods

- [`fcaR::RuleSet$[()`](https://neuroimaginador.github.io/fcaR/reference/RuleSet.html#method-%5B)
- [`fcaR::RuleSet$cardinality()`](https://neuroimaginador.github.io/fcaR/reference/RuleSet.html#method-cardinality)
- [`fcaR::RuleSet$confidence()`](https://neuroimaginador.github.io/fcaR/reference/RuleSet.html#method-confidence)
- [`fcaR::RuleSet$filter()`](https://neuroimaginador.github.io/fcaR/reference/RuleSet.html#method-filter)
- [`fcaR::RuleSet$get_LHS_matrix()`](https://neuroimaginador.github.io/fcaR/reference/RuleSet.html#method-get_LHS_matrix)
- [`fcaR::RuleSet$get_RHS_matrix()`](https://neuroimaginador.github.io/fcaR/reference/RuleSet.html#method-get_RHS_matrix)
- [`fcaR::RuleSet$get_attributes()`](https://neuroimaginador.github.io/fcaR/reference/RuleSet.html#method-get_attributes)
- [`fcaR::RuleSet$get_implications()`](https://neuroimaginador.github.io/fcaR/reference/RuleSet.html#method-get_implications)
- [`fcaR::RuleSet$get_quality()`](https://neuroimaginador.github.io/fcaR/reference/RuleSet.html#method-get_quality)
- [`fcaR::RuleSet$is_empty()`](https://neuroimaginador.github.io/fcaR/reference/RuleSet.html#method-is_empty)
- [`fcaR::RuleSet$size()`](https://neuroimaginador.github.io/fcaR/reference/RuleSet.html#method-size)
- [`fcaR::RuleSet$to_latex()`](https://neuroimaginador.github.io/fcaR/reference/RuleSet.html#method-to_latex)

------------------------------------------------------------------------

### Method `new()`

Initialize an ImplicationSet

#### Usage

    ImplicationSet$new(...)

#### Arguments

- `...`:

  A `rules` object (from `arules`) or named arguments: `name` (string),
  `attributes` (character vector), `lhs` and `rhs` (sparse matrices),
  `I` (incidence matrix).

#### Returns

A new `ImplicationSet` object.

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

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print all implications to text

#### Usage

    ImplicationSet$print()

#### Returns

A string with all the implications in the set.

------------------------------------------------------------------------

### Method `support()`

Compute support of each implication

#### Usage

    ImplicationSet$support()

#### Returns

A vector with the support of each implication

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
      method = c("do_sp", "direct_optimal", "final_ts", "monotonic", "priority"),
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

### Method `to_json()`

Export the implication set to JSON

#### Usage

    ImplicationSet$to_json(file = NULL, return_list = FALSE)

#### Arguments

- `file`:

  (character) The path of the file to save the JSON to.

- `return_list`:

  (logical) If TRUE, returns the list representation instead of the JSON
  string.

#### Returns

A JSON string representing the implication set, or a list if
`return_list` is TRUE.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ImplicationSet$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
