# R6 class for a fuzzy set with sparse internal representation

This class implements the data structure and methods for fuzzy sets.

## Methods

### Public methods

- [`Set$new()`](#method-Set-new)

- [`Set$assign()`](#method-Set-assign)

- [`Set$[()`](#method-Set-bracket)

- [`Set$cardinal()`](#method-Set-cardinal)

- [`Set$get_vector()`](#method-Set-get_vector)

- [`Set$get_attributes()`](#method-Set-get_attributes)

- [`Set$length()`](#method-Set-length)

- [`Set$print()`](#method-Set-print)

- [`Set$to_latex()`](#method-Set-to_latex)

- [`Set$clone()`](#method-Set-clone)

------------------------------------------------------------------------

### Method `new()`

Creator for objects of class `Set`

#### Usage

    Set$new(attributes, M = NULL, ...)

#### Arguments

- `attributes`:

  (character vector) Names of the attributes that will be available in
  the fuzzy set.

- `M`:

  (numeric vector or column `Matrix`) Values (grades) to be assigned to
  the attributes.

- `...`:

  `key` = `value` pairs, where the value `value` is assigned to the
  `key` attribute name.

#### Details

If `M` is omitted and no pair `key` = `value`, the fuzzy set is the
empty set. Later, one can use the `assign` method to assign grades to
any of its attributes.

#### Returns

An object of class `Set`.

------------------------------------------------------------------------

### Method [`assign()`](https://rdrr.io/r/base/assign.html)

Assign grades to attributes in the set

#### Usage

    Set$assign(..., attributes = c(), values = c())

#### Arguments

- `...`:

  `key` = `value` pairs, where the value `value` is assigned to the
  `key` attribute name.

- `attributes`:

  (character vector) Names of the attributes to assign a grade to.

- `values`:

  (numeric vector) Grades to be assigned to the previous `attributes`.

#### Details

One can use both of: `S$assign(A = 1, B = 0.3)`
`S$assign(attributes = c(A, B), values = c(1, 0.3))`.

------------------------------------------------------------------------

### Method `[()`

Get elements by index

#### Usage

    Set$[(indices)

#### Arguments

- `indices`:

  (numeric, logical or character vector) The indices of the elements to
  return. It can be a vector of logicals where `TRUE` elements are to be
  retained.

#### Returns

A `Set` but with only the required elements.

------------------------------------------------------------------------

### Method `cardinal()`

Cardinal of the Set

#### Usage

    Set$cardinal()

#### Returns

the cardinal of the `Set`, counted as the sum of the degrees of each
element.

------------------------------------------------------------------------

### Method `get_vector()`

Internal `Matrix`

#### Usage

    Set$get_vector()

#### Returns

The internal sparse `Matrix` representation of the set.

------------------------------------------------------------------------

### Method `get_attributes()`

Attributes defined for the set

#### Usage

    Set$get_attributes()

#### Returns

A character vector with the names of the attributes.

------------------------------------------------------------------------

### Method [`length()`](https://rdrr.io/r/base/length.html)

Number of attributes

#### Usage

    Set$length()

#### Returns

The number of attributes that are defined for this fuzzy set.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints the set to console

#### Usage

    Set$print(eol = TRUE)

#### Arguments

- `eol`:

  (logical) If `TRUE`, adds an end of line to the output.

#### Returns

A string with the elements of the set and their grades between brackets
.

------------------------------------------------------------------------

### Method `to_latex()`

Write the set in LaTeX format

#### Usage

    Set$to_latex(print = TRUE)

#### Arguments

- `print`:

  (logical) Print to output?

#### Returns

The fuzzy set in LaTeX.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Set$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
S <- Set$new(attributes = c("A", "B", "C"))
S$assign(A = 1)
print(S)
#> {A}
S$to_latex()
#> Note: You must include the following commands in you LaTeX document:
#> \usepackage{amsmath}\newcommand{\el}[2]{\ensuremath{^{#2\!\!}/{#1}}}
#> \left\{\mathrm{A}\right\}

S <- Set$new(c("A", "B", "C"), C = 1, B = 0.5)
S
#> {B [0.5], C}
```
