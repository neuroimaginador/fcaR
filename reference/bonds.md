# Compute bonds between two formal contexts

A bond between two formal contexts \\K_1\\ and \\K_2\\ is a relation
between the objects of \\K_1\\ and the attributes of \\K_2\\, such that
the relation is closed under the derivation operators of both contexts.

## Usage

``` r
bonds(fc1, fc2, method = c("conexp", "mcis"), verbose = FALSE)
```

## Arguments

- fc1:

  (`FormalContext`) The first formal context.

- fc2:

  (`FormalContext`) The second formal context.

- method:

  (character) The method to use. `"conexp"` uses implication-based
  closed set enumeration on the tensor product. `"mcis"` uses a
  backtracking algorithm based on pre-computed concepts (extents of C1
  and intents of C2). Default is `"conexp"`.

- verbose:

  (logical) If TRUE, print progress information.

## Value

A `BondLattice` object whose intents represent the bonds.

## Examples

``` r
# \donttest{
set.seed(42)
mat1 <- matrix(sample(0:1, 15, replace = TRUE), nrow = 5, ncol = 3)
rownames(mat1) <- paste0("O", 1:5)
colnames(mat1) <- paste0("A", 1:3)
fc1 <- FormalContext$new(mat1)

mat2 <- matrix(sample(0:1, 12, replace = TRUE), nrow = 4, ncol = 3)
rownames(mat2) <- paste0("P", 1:4)
colnames(mat2) <- paste0("B", 1:3)
fc2 <- FormalContext$new(mat2)

# Compute bonds returning a lattice
bonds_lattice <- bonds(fc1, fc2, method = "conexp")
bonds_lattice$print()
#> Bond Lattice between two formal contexts:
#> - Context 1 (G1): 5 objects (O1, O2...)
#> - Context 2 (M2): 3 attributes (B1, B2...)
#> - Total Bonds: 65 
# }
```
