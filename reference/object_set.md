# Build an object `Set` from names

Dual of
[`attribute_set()`](https://neuroimaginador.github.io/fcaR/reference/attribute_set.md):
builds a `Set` over `fc$objects`.

## Usage

``` r
object_set(fc, ...)
```

## Arguments

- fc:

  A `FormalContext`.

- ...:

  Object names: a character vector, or several strings.

## Value

A `Set` over `fc$objects`.

## Examples

``` r
if (FALSE) { # \dontrun{
object_set(fc, "Squid Game", "Dark")
} # }
```
