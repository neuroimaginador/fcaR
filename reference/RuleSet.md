# R6 Class for Set of association rules

This class implements the structure needed to store association rules
and the methods associated.

## Methods

### Public methods

- [`RuleSet$new()`](#method-RuleSet-new)

- [`RuleSet$get_attributes()`](#method-RuleSet-get_attributes)

- [`RuleSet$[()`](#method-RuleSet-bracket)

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

- [`RuleSet$clone()`](#method-RuleSet-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize with an optional name

#### Usage

    RuleSet$new(...)

#### Arguments

- `...`:

  See Details.

#### Details

Creates and initialize a new `RuleSet` object. It can be done in two
ways: `initialize(name, attributes, lhs, rhs, quality)` or
`initialize(rules)`

In the first way, the only mandatory argument is `attributes`,
(character vector) which is a vector of names of the attributes on which
we define the rules.

The other way is used to initialize the `RuleSet` object from a `rules`
object from package `arules`.

#### Returns

A new `RuleSet` object.

------------------------------------------------------------------------

### Method `get_attributes()`

Get the names of the attributes

#### Usage

    RuleSet$get_attributes()

#### Returns

A character vector with the names of the attributes used in the
implications.

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

  An `RuleSet` object, or a pair `lhs`, `rhs` of `dgCMatrix`.

#### Returns

Nothing, just updates the internal field.

------------------------------------------------------------------------

### Method `cardinality()`

Cardinality: Number of implications in the set

#### Usage

    RuleSet$cardinality()

#### Returns

The cardinality of the implication set.

------------------------------------------------------------------------

### Method `is_empty()`

Empty set

#### Usage

    RuleSet$is_empty()

#### Returns

`TRUE` if the set of implications is empty, `FALSE` otherwise.

------------------------------------------------------------------------

### Method `size()`

Size: number of attributes in each of LHS and RHS

#### Usage

    RuleSet$size()

#### Returns

A vector with two components: the number of attributes present in each
of the LHS and RHS of each implication in the set.

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

A string in LaTeX format that prints nicely all the implications.

------------------------------------------------------------------------

### Method `get_LHS_matrix()`

Get internal LHS matrix

#### Usage

    RuleSet$get_LHS_matrix()

#### Returns

A sparse matrix representing the LHS of the implications in the set.

------------------------------------------------------------------------

### Method `get_RHS_matrix()`

Get internal RHS matrix

#### Usage

    RuleSet$get_RHS_matrix()

#### Returns

A sparse matrix representing the RHS of the implications in the set.

------------------------------------------------------------------------

### Method [`filter()`](https://rdrr.io/r/stats/filter.html)

Filter implications by attributes in LHS and RHS

#### Usage

    RuleSet$filter(lhs = NULL, rhs = NULL, drop = FALSE)

#### Arguments

- `lhs`:

  (character vector) Names of the attributes to filter the LHS by. If
  `NULL`, no filtering is done on the LHS.

- `rhs`:

  (character vector) Names of the attributes to filter the RHS by. If
  `NULL`, no filtering is done on the RHS.

- `drop`:

  (logical) Remove the rest of attributes in RHS?

#### Returns

An `RuleSet` that is a subset of the current set, only with those rules
which has the attributes in `lhs` and `rhs` in their LHS and RHS,
respectively.

------------------------------------------------------------------------

### Method `get_implications()`

#### Usage

    RuleSet$get_implications()

------------------------------------------------------------------------

### Method `support()`

Compute support of each implication

#### Usage

    RuleSet$support()

#### Returns

A vector with the support of each implication

------------------------------------------------------------------------

### Method `confidence()`

#### Usage

    RuleSet$confidence()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RuleSet$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
