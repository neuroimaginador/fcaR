# Verify if a relation is a bond between two formal contexts

A bond between two formal contexts \\K_1 = (G_1, M_1, I_1)\\ and \\K_2 =
(G_2, M_2, I_2)\\ is a relation \\R \subseteq G_1 \times M_2\\ such that
every row of \\R\\ is an intent of \\K_2\\ and every column of \\R\\ is
an extent of \\K_1\\.

## Usage

``` r
is_bond(fc1, fc2, relation)
```

## Arguments

- fc1:

  (`FormalContext`) The first formal context.

- fc2:

  (`FormalContext`) The second formal context.

- relation:

  (matrix) A binary matrix or `FormalContext` representing the relation
  between objects of \\fc1\\ and attributes of \\fc2\\.

## Value

`TRUE` if the relation is a bond, `FALSE` otherwise.
