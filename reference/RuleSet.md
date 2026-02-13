# R6 class for a Rule Set

This class implements a generic rule set (LHS -\> RHS), serving as the
parent class for `ImplicationSet`. It provides common functionality for
managing, filtering, and exporting rules.

## Methods

### Public methods

- [`RuleSet$new()`](#method-RuleSet-new)

- [`RuleSet$get_attributes()`](#method-RuleSet-get_attributes)

- [`RuleSet$[()`](#method-RuleSet-%5B)

- [`RuleSet$to_arules()`](#method-RuleSet-to_arules)

- [`RuleSet$add()`](#method-RuleSet-add)

- [`RuleSet$cardinality()`](#method-RuleSet-cardinality)

- [`RuleSet$is_empty()`](#method-RuleSet-is_empty)

- [`RuleSet$size()`](#method-RuleSet-size)

- [`RuleSet$print()`](#method-RuleSet-print)

- [`RuleSet$get_quality()`](#method-RuleSet-get_quality)

- [`RuleSet$to_latex()`](#method-RuleSet-to_latex)

- [`RuleSet$get_LHS_matrix()`](#method-RuleSet-get_LHS_matrix)

- [`RuleSet$get_RHS_matrix()`](#method-RuleSet-get_RHS_matrix)

- [`RuleSet$filter()`](#method-RuleSet-filter)

- [`RuleSet$get_implications()`](#method-RuleSet-get_implications)

- [`RuleSet$support()`](#method-RuleSet-support)

- [`RuleSet$confidence()`](#method-RuleSet-confidence)

- [`RuleSet$to_json()`](#method-RuleSet-to_json)

- [`RuleSet$clone()`](#method-RuleSet-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a RuleSet

#### Usage

    RuleSet$new(...)

#### Arguments

- `...`:

  A `rules` object (from `arules`) or named arguments: `name` (string),
  `attributes` (character vector), `lhs` and `rhs` (sparse matrices),
  `I` (incidence matrix), `quality` (data.frame), `confidence` (numeric
  vector, backward compat).

#### Returns

A new `RuleSet` object.

------------------------------------------------------------------------

### Method `get_attributes()`

Get the names of the attributes

#### Usage

    RuleSet$get_attributes()

#### Returns

A character vector with the names of the attributes used in the rules.

------------------------------------------------------------------------

### Method `[()`

Get a subset of the rule set

#### Usage

    RuleSet$[(idx)

#### Arguments

- `idx`:

  (integer or logical vector) Indices of the rules to extract or remove.
  If logical vector, only `TRUE` elements are retained and the rest
  discarded.

#### Returns

A new `RuleSet` with only the rules given by the `idx` indices.

------------------------------------------------------------------------

### Method `to_arules()`

Convert to arules format

#### Usage

    RuleSet$to_arules(quality = TRUE)

#### Arguments

- `quality`:

  (logical) Compute/include the interest measures for each rule?

#### Returns

A `rules` object as used by package `arules`.

------------------------------------------------------------------------

### Method `add()`

Add a precomputed rule set

#### Usage

    RuleSet$add(...)

#### Arguments

- `...`:

  A `RuleSet` object, or a pair `lhs`, `rhs` of `dgCMatrix`.

#### Returns

Nothing, just updates the internal field.

------------------------------------------------------------------------

### Method `cardinality()`

Cardinality: Number of rules in the set

#### Usage

    RuleSet$cardinality()

#### Returns

The cardinality of the rule set.

------------------------------------------------------------------------

### Method `is_empty()`

Empty set

#### Usage

    RuleSet$is_empty()

#### Returns

`TRUE` if the set of rules is empty, `FALSE` otherwise.

------------------------------------------------------------------------

### Method `size()`

Size: number of attributes in each of LHS and RHS

#### Usage

    RuleSet$size()

#### Returns

A matrix with two columns: the number of attributes present in each of
the LHS and RHS of each rule.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print all rules to text

#### Usage

    RuleSet$print()

#### Returns

A string with all the rules in the set.

------------------------------------------------------------------------

### Method `get_quality()`

Get quality metrics

#### Usage

    RuleSet$get_quality()

#### Returns

A data.frame with the quality metrics for each rule.

------------------------------------------------------------------------

### Method `to_latex()`

Export to LaTeX

#### Usage

    RuleSet$to_latex(
      print = TRUE,
      ncols = 1,
      numbered = TRUE,
      numbers = seq_len(self$cardinality())
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

A string in LaTeX format that prints nicely all the rules.

------------------------------------------------------------------------

### Method `get_LHS_matrix()`

Get internal LHS matrix

#### Usage

    RuleSet$get_LHS_matrix()

#### Returns

A sparse matrix representing the LHS of the rules in the set.

------------------------------------------------------------------------

### Method `get_RHS_matrix()`

Get internal RHS matrix

#### Usage

    RuleSet$get_RHS_matrix()

#### Returns

A sparse matrix representing the RHS of the rules in the set.

------------------------------------------------------------------------

### Method [`filter()`](https://rdrr.io/r/stats/filter.html)

Filter rules by attributes in LHS and RHS

#### Usage

    RuleSet$filter(
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

A `RuleSet` (or subclass) that is a subset of the current set, only with
those rules which have the attributes in `lhs` and `rhs` in their LHS
and RHS, respectively.

------------------------------------------------------------------------

### Method `get_implications()`

Extract the implications with confidence 1

#### Usage

    RuleSet$get_implications()

#### Returns

An `ImplicationSet` object containing only the rules with confidence 1.

------------------------------------------------------------------------

### Method `support()`

Compute support of each rule

#### Usage

    RuleSet$support()

#### Returns

A vector with the support of each rule.

------------------------------------------------------------------------

### Method `confidence()`

Compute the confidence of each rule

#### Usage

    RuleSet$confidence()

#### Returns

A numeric vector with the confidence of each rule.

------------------------------------------------------------------------

### Method `to_json()`

Export the rule set to JSON

#### Usage

    RuleSet$to_json(file = NULL, return_list = FALSE)

#### Arguments

- `file`:

  (character) The path of the file to save the JSON to.

- `return_list`:

  (logical) If TRUE, returns the list representation instead of the JSON
  string.

#### Returns

A JSON string representing the rule set, or a list if `return_list` is
TRUE.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RuleSet$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
