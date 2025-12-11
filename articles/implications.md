# Working with ImplicationSets

## Introduction

In this vignette, we present the main functionalities and data
structures of the `fcaR` package when working with implications in FCA.

We load the `fcaR` package by:

``` r
library(fcaR)
```

## Datasets

We are going to work with two datasets, a crisp one and a fuzzy one.

The classical (binary) dataset is the well-known `planets` formal
context, presented in

> Wille R (1982). “Restructuring Lattice Theory: An Approach Based on
> Hierarchies of Concepts.” In Ordered Sets, pp. 445–470. Springer.

``` r
knitr::kable(planets, format = "html", booktabs = TRUE)
```

|         | small | medium | large | near | far | moon | no_moon |
|:--------|------:|-------:|------:|-----:|----:|-----:|--------:|
| Mercury |     1 |      0 |     0 |    1 |   0 |    0 |       1 |
| Venus   |     1 |      0 |     0 |    1 |   0 |    0 |       1 |
| Earth   |     1 |      0 |     0 |    1 |   0 |    1 |       0 |
| Mars    |     1 |      0 |     0 |    1 |   0 |    1 |       0 |
| Jupiter |     0 |      0 |     1 |    0 |   1 |    1 |       0 |
| Saturn  |     0 |      0 |     1 |    0 |   1 |    1 |       0 |
| Uranus  |     0 |      1 |     0 |    0 |   1 |    1 |       0 |
| Neptune |     0 |      1 |     0 |    0 |   1 |    1 |       0 |
| Pluto   |     1 |      0 |     0 |    0 |   1 |    1 |       0 |

The other formal context is fuzzy and is defined by the following matrix
I:

``` r
knitr::kable(I, format = "html", booktabs = TRUE)
```

|     |  P1 |  P2 |  P3 |  P4 |  P5 |  P6 |
|:----|----:|----:|----:|----:|----:|----:|
| O1  | 0.0 | 0.0 | 0.5 | 0.5 | 1.0 |   0 |
| O2  | 1.0 | 1.0 | 1.0 | 0.0 | 0.0 |   0 |
| O3  | 0.5 | 0.5 | 0.0 | 0.0 | 0.0 |   1 |
| O4  | 0.0 | 0.0 | 0.0 | 1.0 | 0.5 |   0 |
| O5  | 0.0 | 0.0 | 1.0 | 0.5 | 0.0 |   0 |
| O6  | 0.5 | 0.5 | 0.0 | 0.0 | 0.0 |   1 |

## Working with ImplicationSets

Although `ImplicationSet` objects can be created *ad hoc*, the usual way
to get implications is by the application of the NextClosure algorithm
to a `FormalContext` object.

Thus, let us create different formal contexts with the previous
datasets:

``` r
fc_planets <- FormalContext$new(planets)
fc_I <- FormalContext$new(I)
```

### Extraction of the canonical basis of implications

The function `find_implications()` use the NextClosure algorithm in a
formal context to extract the canonical basis of implications:

``` r
fc_planets$find_implications()
fc_I$find_implications()
```

We can inspect the implications by doing:

``` r
fc_planets$implications
#> Implication set with 10 implications.
#> Rule 1: {no_moon} -> {small, near}
#> Rule 2: {far} -> {moon}
#> Rule 3: {near} -> {small}
#> Rule 4: {large} -> {far, moon}
#> Rule 5: {medium} -> {far, moon}
#> Rule 6: {medium, large, far, moon} -> {small, near, no_moon}
#> Rule 7: {small, near, moon, no_moon} -> {medium, large, far}
#> Rule 8: {small, near, far, moon} -> {medium, large, no_moon}
#> Rule 9: {small, large, far, moon} -> {medium, near, no_moon}
#> Rule 10: {small, medium, far, moon} -> {large, near, no_moon}
fc_I$implications
#> Implication set with 12 implications.
#> Rule 1: {P6 [0.5]} -> {P1 [0.5], P2 [0.5], P6}
#> Rule 2: {P5 [0.5]} -> {P4 [0.5]}
#> Rule 3: {P3 [0.5], P4 [0.5], P5 [0.5]} -> {P5}
#> Rule 4: {P3 [0.5], P4} -> {P3}
#> Rule 5: {P3, P4 [0.5], P5} -> {P4}
#> Rule 6: {P2 [0.5]} -> {P1 [0.5]}
#> Rule 7: {P1 [0.5]} -> {P2 [0.5]}
#> Rule 8: {P1 [0.5], P2 [0.5], P4 [0.5]} -> {P1, P2, P3, P4, P5, P6}
#> Rule 9: {P1 [0.5], P2 [0.5], P3 [0.5]} -> {P1, P2, P3}
#> Rule 10: {P1 [0.5], P2} -> {P1}
#> Rule 11: {P1, P2 [0.5]} -> {P2}
#> Rule 12: {P1, P2, P3, P6} -> {P4, P5}
```

Internally, an `ImplicationSet` consists of two matrices (one for the
left-hand sides and the other for the right-hand sides of the rules). We
can get these (sparse) matrices as:

``` r
fc_planets$implications$get_LHS_matrix()
#> 7 x 10 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 10 column names '1', '2', '3' ... ]]
#>                            
#> small   . . . . . . 1 1 1 1
#> medium  . . . . 1 1 . . . 1
#> large   . . . 1 . 1 . . 1 .
#> near    . . 1 . . . 1 1 . .
#> far     . 1 . . . 1 . 1 1 1
#> moon    . . . . . 1 1 1 1 1
#> no_moon 1 . . . . . 1 . . .
fc_planets$implications$get_RHS_matrix()
#> 7 x 10 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 10 column names '1', '2', '3' ... ]]
#>                            
#> small   1 . 1 . . 1 . . . .
#> medium  . . . . . . 1 1 1 .
#> large   . . . . . . 1 1 . 1
#> near    1 . . . . 1 . . 1 1
#> far     . . . 1 1 . 1 . . .
#> moon    . 1 . 1 1 . . . . .
#> no_moon . . . . . 1 . 1 1 1
```

The main practical use of an `ImplicationSet` is to compute the closure
of a set of attributes, by using the `closure()` function:

``` r
# Let us build a set of attributes
S <- Set$new(attributes = fc_planets$attributes)
S$assign(large = 1, far = 1)
S
#> {large, far}

fc_planets$implications$closure(S)$closure
#> {large, far, moon}
```

### Validity of implications

We can check if an `ImplicationSet` holds in a `FormalContext` by using
the `%holds_in%` operator:

``` r
# Let us clone the implication basis
imps <- fc_planets$implications$clone()
imps %holds_in% fc_planets
#>  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
```

Each component of this vector represents whether the corresponding
implication holds in the formal context. In this case, as the
`ImplicationSet` is the Duquenne-Guigues basis for the `FormalContext`,
all implications hold.

Conversely, we can check if a list of attribute sets (or a formal
context) respects an `ImplicationSet`, via the `%respects%` operator:

``` r
fc_planets %respects% imps
#> 9 x 10 sparse Matrix of class "lgCMatrix"
#>   [[ suppressing 10 column names 'imp_01', 'imp_02', 'imp_03' ... ]]
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

The result is a matrix where each row correspond to a attribute set and
each column to an implication. An element is `TRUE` if the corresponding
set respects the corresponding implication. If the first argument is a
`FormalContext`, the function will consider the set of attributes of
each object.

### Cardinality, size and support of the implication set

Some quantities can be computed for an `ImplicationSet`:

- Cardinality: the number of implications in the set

``` r
fc_planets$implications$cardinality()
#> [1] 10
```

- Size: The number of attributes in the LHS and the RHS of each
  implication

``` r
sizes <- fc_planets$implications$size()
# Total number of attributes in the LHS and the RHS
colSums(sizes)
#> LHS RHS 
#>  25  23
```

- Support: The proportion of objects in the formal context whose
  attributes contain the LHS of a particular rule

``` r
fc_planets$implications$support()
#>  [1] 0.2222222 0.5555556 0.4444444 0.2222222 0.2222222 0.0000000 0.0000000
#>  [8] 0.0000000 0.0000000 0.0000000
```

### Export to LaTeX

A nice feature is the ability to export an `ImplicationSet` to LaTeX
format:

``` r
fc_planets$implications$to_latex()
#> Note: You must include the following commands in you LaTeX document:
#> \usepackage{amsmath}\newcommand{\el}[2]{\ensuremath{^{#2\!\!}/{#1}}}
#> \begin{longtable*}{rrcl}
#> 1: &\left\{\mathrm{no\_moon}\right\}&\ensuremath{\Rightarrow}&\left\{\mathrm{small}, \mathrm{near}\right\}\\
#> 2: &\left\{\mathrm{far}\right\}&\ensuremath{\Rightarrow}&\left\{\mathrm{moon}\right\}\\
#> 3: &\left\{\mathrm{near}\right\}&\ensuremath{\Rightarrow}&\left\{\mathrm{small}\right\}\\
#> 4: &\left\{\mathrm{large}\right\}&\ensuremath{\Rightarrow}&\left\{\mathrm{far}, \mathrm{moon}\right\}\\
#> 5: &\left\{\mathrm{medium}\right\}&\ensuremath{\Rightarrow}&\left\{\mathrm{far}, \mathrm{moon}\right\}\\
#> 6: &\left\{\mathrm{medium}, \mathrm{large}, \mathrm{far}, \mathrm{moon}\right\}&\ensuremath{\Rightarrow}&\left\{\mathrm{small}, \mathrm{near}, \mathrm{no\_moon}\right\}\\
#> 7: &\left\{\mathrm{small}, \mathrm{near}, \mathrm{moon}, \mathrm{no\_moon}\right\}&\ensuremath{\Rightarrow}&\left\{\mathrm{medium}, \mathrm{large}, \mathrm{far}\right\}\\
#> 8: &\left\{\mathrm{small}, \mathrm{near}, \mathrm{far}, \mathrm{moon}\right\}&\ensuremath{\Rightarrow}&\left\{\mathrm{medium}, \mathrm{large}, \mathrm{no\_moon}\right\}\\
#> 9: &\left\{\mathrm{small}, \mathrm{large}, \mathrm{far}, \mathrm{moon}\right\}&\ensuremath{\Rightarrow}&\left\{\mathrm{medium}, \mathrm{near}, \mathrm{no\_moon}\right\}\\
#> 10: &\left\{\mathrm{small}, \mathrm{medium}, \mathrm{far}, \mathrm{moon}\right\}&\ensuremath{\Rightarrow}&\left\{\mathrm{large}, \mathrm{near}, \mathrm{no\_moon}\right\}\\
#> \end{longtable*}
```

### Filtering of implications

Sometimes it is needed to work with a subset of the implications, using
only the implications that fulfill certain conditions:

``` r
# Implications with P1 and P2 in the LHS and P5 in the RHS
fc_I$implications$filter(
  lhs = c("P1", "P2"),
  rhs = "P5"
)
#> Implication set with 2 implications.
#> Rule 1: {P1 [0.5], P2 [0.5], P4 [0.5]} -> {P1, P2, P3, P4, P5, P6}
#> Rule 2: {P1, P2, P3, P6} -> {P4, P5}
```

### Simplification Logic

In this package, we have implemented logic tools to operate on the
implications.

First, some simplification rules have been developed, named *reduction*,
*composition*, *generalization* and *simplification*, that can be
applied using the `apply_rules()` function:

``` r
fc_I$implications$apply_rules(rules = c(
  "composition",
  "simplification"
))
#> Processing batch
#> --> Composition: from 12 to 12 in 0.005 secs.
#> --> Simplification: from 12 to 12 in 0.037 secs.
#> Batch took 0.044 secs.
```

This enables the reduction of the cardinality and/or the size of the
`ImplicationSet`.

In addition, the “simplification” rule to remove redundancies can be
used in the computation of the closure of a set, to provide a reduced
set of implications that is inferred from the set of attributes:

``` r
# Let us build a set of attributes
S <- Set$new(attributes = fc_planets$attributes)
S$assign(large = 1, far = 1)
S
#> {large, far}

fc_planets$implications$closure(S, reduce = TRUE)
#> $closure
#> {large, far, moon}
#> 
#> $implications
#> Implication set with 4 implications.
#> Rule 1: {medium} -> {small, near, no_moon}
#> Rule 2: {small} -> {medium, near, no_moon}
#> Rule 3: {no_moon} -> {small, near}
#> Rule 4: {near} -> {small}
```

### Entailment and equivalence of implications

We can check if a given `ImplicationSet` follows from another one:

``` r
# imps is the basis
imps <- fc_planets$implications$clone()
imps2 <- imps$clone()
# imps2 is an equivalent set of implications
# where we have removed redundancies
imps2$apply_rules(c("simp", "rsimp"))
#> Processing batch
#> --> Simplification: from 10 to 10 in 0.019 secs.
#> --> Right Simplification: from 10 to 10 in 0.048 secs.
#> Batch took 0.07 secs.
# Any implication in imps2 follows from imps
imps %entails% imps2
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE  TRUE
# And viceversa
imps2 %entails% imps
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE  TRUE
```

We can also check if the two sets of implications are equivalent:

``` r
imps %~% imps2
#> [1] TRUE
# If we remove any implication from imps2,
# they will not be equivalent
imps %~% imps2[1:9]
#> [1] FALSE
```

### Recommendations

One of the final applications of an `ImplicationSet` is the easy
development of a recommendation system where, from an attribute set, the
system would infer the value to other attribute. This is done by the
`recommend()` function, which internally computes the closure of the
attribute set:

``` r
S <- Set$new(attributes = fc_I$attributes)
S$assign(P1 = 1, P4 = 0.5)

fc_I$implications$recommend(S, attribute_filter = c("P3", "P5"))
#> P3 P5 
#>  1  1
```
