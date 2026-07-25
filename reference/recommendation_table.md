# Pretty table of recommendation scores

Turns the named numeric vector from `implications$recommend()` into a
sorted data frame that is easy to read in the console or in Quarto.

## Usage

``` r
recommendation_table(recoms, drop_zero = TRUE, query = NULL)
```

## Arguments

- recoms:

  Named numeric vector (as returned by `$recommend()`).

- drop_zero:

  Logical; if `TRUE` (default), drop attributes with score 0 (not
  implied / not recommended).

- query:

  Optional character vector of attributes that were in the original
  query. If set, adds a `role` column: `"query"`, `"recommended"`, or
  `"not implied"`.

## Value

A data frame with columns `attribute`, `score`, and optionally `role`.
Rows are ordered by decreasing score, then name.

## Examples

``` r
if (FALSE) { # \dontrun{
S <- attribute_set(fc, "Romantic", "Slow_burn")
recoms <- fc$implications$recommend(S, attribute_filter = fc$attributes)
recommendation_table(recoms)
} # }
```
