# Implications that hold in a Formal Context

Implications that hold in a Formal Context

## Usage

``` r
imps %holds_in% fc
```

## Arguments

- imps:

  (`ImplicationSet`) The set of implications to test if hold in the
  formal context.

- fc:

  (`FormalContext`) A formal context where to test if the implications
  hold.

## Value

A logical vector, indicating if each implication holds in the formal
context.

## Examples

``` r
fc <- FormalContext$new(planets)
fc$find_implications()
imps <- fc$implications$clone()
imps %holds_in% fc
#>  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
```
