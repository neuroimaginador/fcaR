# Build an attribute `Set` from names

Replaces the common two-step pattern of creating and assigning
attributes manually with a single call. Useful for implication closures
and `$recommend()`.

## Usage

``` r
attribute_set(fc, ...)
```

## Arguments

- fc:

  A `FormalContext`.

- ...:

  Attribute names: a character vector, or several strings.

## Value

A `Set` over `fc$attributes`.

## Examples

``` r
if (FALSE) { # \dontrun{
S <- attribute_set(fc, "Romantic", "Slow_burn")
fc$implications$closure(S)
} # }
```
