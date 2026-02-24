# Compute bonds between two formal contexts

A bond between two formal contexts \\K_1\\ and \\K_2\\ is a relation
between the objects of \\K_1\\ and the attributes of \\K_2\\, such that
the relation is closed under the derivation operators of both contexts.

## Usage

``` r
bonds(fc1, fc2)
```

## Arguments

- fc1:

  (`FormalContext`) The first formal context.

- fc2:

  (`FormalContext`) The second formal context.

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
bonds_lattice <- bonds(fc1, fc2)
bonds_lattice$print()
#> Bond Lattice between two formal contexts:
#> - Context 1 (G1): 5 objects (O1, O2...)
#> - Context 2 (M2): 3 attributes (B1, B2...)
#> - Total Bonds: 65 
#> - Logical Affinity (Log-Bond): 0.9947 
#> - Interaction Entropy: 0.4944 
#> - Structural Complexity (JI/Bonds): 0.1385 
#> - Core Agreement Ratio: 0 
#> - Average Bond Stability: 0.2599 
#> - Dilworth's Width: 13 (Index: 0.2000)
#> - Order Dimension: 10 (Index: 1.6605)
# }
```
