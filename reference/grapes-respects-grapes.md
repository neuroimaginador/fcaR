# Check if Set or FormalContext respects an ImplicationSet

Check if Set or FormalContext respects an ImplicationSet

## Usage

``` r
set %respects% imps
```

## Arguments

- set:

  (list of `Set`s, or a `FormalContext`) The sets of attributes to check
  whether they respect the `ImplicationSet`.

- imps:

  (`ImplicationSet`) The set of implications to check.

## Value

A logical matrix with as many rows as `Set`s and as many columns as
implications in the `ImplicationSet`. A `TRUE` in element (i, j) of the
result means that the i-th `Set` respects the j-th implication of the
`ImplicationSet`.

## Examples

``` r
fc <- FormalContext$new(planets)
fc$find_implications()
imps <- fc$implications$clone()
fc %respects% imps
#> 9 x 10 sparse Matrix of class "lgCMatrix"
#>   [[ suppressing 10 column names ‘imp_01’, ‘imp_02’, ‘imp_03’ ... ]]
#>                          
#> set_1 | | | | | | | | | |
#> set_2 | | | | | | | | | |
#> set_3 | | | | | | | | | |
#> set_4 | | | | | | | | | |
#> set_5 | | | | | | | | | |
#> set_6 | | | | | | | | | |
#> set_7 | | | | | | | | | |
#> set_8 | | | | | | | | | |
#> set_9 | | | | | | | | | |
```
