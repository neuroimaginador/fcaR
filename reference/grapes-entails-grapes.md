# Entailment between implication sets

Entailment between implication sets

## Usage

``` r
imps %entails% imps2
```

## Arguments

- imps:

  (`ImplicationSet`) A set of implications.

- imps2:

  (`ImplicationSet`) A set of implications which is tested to check if
  it follows semantically from `imps`.

## Value

A logical vector, where element k is `TRUE` if the k-th implication in
`imps2` follows from `imps`.

## Examples

``` r
fc <- FormalContext$new(planets)
fc$find_implications()
imps <- fc$implications[1:4]$clone()
imps2 <- fc$implications[3:6]$clone()
imps %entails% imps2
#>      [,1] [,2]  [,3]  [,4]
#> [1,] TRUE TRUE FALSE FALSE
```
