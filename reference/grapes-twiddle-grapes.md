# Equivalence of sets of implications

Equivalence of sets of implications

## Usage

``` r
imps %~% imps2
```

## Arguments

- imps:

  A `ImplicationSet`.

- imps2:

  Another `ImplicationSet`.

## Value

`TRUE` of and only if `imps` and `imps2` are equivalent, that is, if
every implication in `imps` follows from `imps2` and viceversa.

## Examples

``` r
fc <- FormalContext$new(planets)
fc$find_implications()
imps <- fc$implications$clone()
imps2 <- imps$clone()
imps2$apply_rules(c("simp", "rsimp"))
#> Processing batch
#> --> Simplification: from 10 to 10 in 0.015 secs.
#> --> Right Simplification: from 10 to 10 in 0.037 secs.
#> Batch took 0.053 secs. 
imps %~% imps2
#> [1] TRUE
imps %~% imps2[1:9]
#> [1] FALSE
```
