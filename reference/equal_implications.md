# Equality of sets of implications

Tests whether two `ImplicationSet` or `RuleSet` objects represent the
exact same set of implications, regardless of the order in which they
appear. It uses a prefix tree (Trie) over the premises (LHS) for fast
matching.

## Usage

``` r
equal_implications(imps1, imps2)
```

## Arguments

- imps1:

  (`RuleSet` or `ImplicationSet`) The first set of implications.

- imps2:

  (`RuleSet` or `ImplicationSet`) The second set of implications.

## Value

`TRUE` if both sets contain the exact same implications, `FALSE`
otherwise.

## Examples

``` r
fc <- FormalContext$new(planets)
fc$find_implications()
imps1 <- fc$implications$clone()
# Shuffle the implications
imps2 <- imps1[sample(imps1$cardinality())]
equal_implications(imps1, imps2)
#> [1] TRUE
imps1 %==% imps2
#> [1] TRUE
```
