# Matrix Factorization

``` r

library(fcaR)
```

## Introduction

**Boolean Matrix Factorization (BMF)** is a technique used to decompose
a binary matrix $`I`$ (representing a formal context) into two smaller
binary matrices:

1.  **Object-Factor Matrix ($`A`$)** ($`n \times k`$): Describes to what
    degree each object belongs to or possesses each factor.
2.  **Factor-Attribute Matrix ($`B`$)** ($`k \times m`$): Describes to
    what degree each factor exhibits or manifests as specific
    attributes.

The goal is that the Boolean product of these matrices approximates the
original data:
``` math
 I \approx A \circ B 
```
where the composition $`\circ`$ is the standard Boolean matrix product
(i.e., using logical OR for addition and logical AND for
multiplication). In the context of Formal Concept Analysis (FCA), this
corresponds to finding a small subset of formal concepts (which act as
factors) that can explain or reconstruct the original data.

> \[!NOTE\] Currently, `fcaR` only supports factorization for Boolean
> (binary) contexts. If you have a fuzzy context, you must binarize it
> first.

`fcaR` implements several state-of-the-art Boolean Matrix Factorization
algorithms: \* **GreConD:** A greedy algorithm that extracts factors
sequentially based on formal concepts. \* **ASSO:** An association
rules-based heuristic algorithm. \* **RSF / RSF-ES:** Algorithms that
construct factors based on the reduction of support. \* **GreEss:** An
algorithm focusing on essential entries of the matrix. \* **PaNDa+:**
Algorithms (MDL, ASSO-style, and Weighted) that use the Minimum
Description Length principle. \* **Hyper / Hyper+:** Lattice-based
factorization methods.

------------------------------------------------------------------------

## 1. Boolean Matrix Factorization with GreConD

Let’s use a binary dataset describing different dog breeds and their
characteristics. We want to see if we can reduce these breeds to a few
“archetypes” (factors).

### Dataset Creation

We define a binary matrix where each row represents a dog breed and each
column represents a characteristic:

``` r

# Create a binary matrix (5 breeds x 5 attributes)
I <- matrix(c(
  1, 1, 0, 0, 0, # Labrador: Friendly, Playful
  1, 1, 0, 0, 0, # Golden Retriever: Friendly, Playful
  0, 0, 1, 1, 0, # German Shepherd: Guard, Aggressive
  0, 0, 1, 1, 0, # Rottweiler: Guard, Aggressive
  1, 0, 0, 0, 1  # Chihuahua: Friendly, Small
), nrow = 5, byrow = TRUE)

rownames(I) <- c("Labrador", "Golden Ret.", "G. Shepherd", "Rottweiler", "Chihuahua")
colnames(I) <- c("Friendly", "Playful", "Guard", "Aggressive", "Small")

# Initialize the FormalContext
fc <- FormalContext$new(I)
print(fc)
#> FormalContext with 5 objects and 5 attributes.
#>              Friendly  Playful  Guard  Aggressive  Small  
#>     Labrador     X        X                               
#>  Golden Ret.     X        X                               
#>  G. Shepherd                      X         X             
#>   Rottweiler                      X         X             
#>    Chihuahua     X                                   X
```

### Performing Factorization

We apply the **GreConD** algorithm to decompose the context:

``` r

# Factorize using GreConD
factors <- fc$factorize(method = "GreConD")
#> Warning in Matrix.DeprecatedCoerce(cd1, cd2): 'as(<dgCMatrix>, "ngCMatrix")' is deprecated.
#> Use 'as(., "nMatrix")' instead.
#> See help("Deprecated") and help("Matrix-deprecated").

# The result contains two new FormalContext objects
A <- factors$object_factor
B <- factors$factor_attribute
```

### Interpreting the Factors

Let’s print the two factor matrices to see how they describe our data:

#### The Object-Factor Matrix ($`A`$)

This matrix connects the original dog breeds (objects) to the discovered
latent factors:

``` r

print(A$incidence())
#>             F1 F2 F3
#> Labrador     1  0  0
#> Golden Ret.  1  0  0
#> G. Shepherd  0  1  0
#> Rottweiler   0  1  0
#> Chihuahua    0  0  1
```

- **Factor 1 (`F1`)** is present in `Labrador`, `Golden Ret.`, and
  `Chihuahua`.
- **Factor 2 (`F2`)** is present in `G. Shepherd` and `Rottweiler`.
- **Factor 3 (`F3`)** is present only in `Chihuahua`.

#### The Factor-Attribute Matrix ($`B`$)

This matrix connects the latent factors to the original characteristics
(attributes):

``` r

print(B$incidence())
#>    Friendly Playful Guard Aggressive Small
#> F1        1       1     0          0     0
#> F2        0       0     1          1     0
#> F3        1       0     0          0     1
```

- **Factor 1 (`F1`)** is characterized by the attribute `Friendly`.
- **Factor 2 (`F2`)** is characterized by the attributes `Guard` and
  `Aggressive`.
- **Factor 3 (`F3`)** is characterized by the attribute `Small`.

Combining these observations: 1. **Factor 1 (`F1`)** represents the
archetype of a **“Friendly Dog”** (which Labradors, Goldens, and
Chihuahuas are). 2. **Factor 2 (`F2`)** represents the archetype of a
**“Guard/Protective Dog”** (exhibited by German Shepherds and
Rottweilers). 3. **Factor 3 (`F3`)** represents the archetype of a
**“Small Dog”** (which distinguishes the Chihuahua from the others).

------------------------------------------------------------------------

## 2. Other Algorithms: ASSO

For large binary datasets, **ASSO** is a classic alternative heuristic.
It uses pairwise association confidence to generate candidate factors.

``` r

# Factorize using ASSO
res_asso <- fc$factorize(method = "ASSO", threshold = 0.6)

# Print the resulting factor-attribute matrix
print(res_asso$factor_attribute$incidence())
#>    Friendly Playful Guard Aggressive Small
#> F1        1       1     0          0     0
#> F2        0       0     1          1     0
#> F3        1       0     0          0     1
```

------------------------------------------------------------------------

## References

1.  **Belohlavek, R. (2010).** Discovery of optimal factors in binary
    data via a novel method of matrix decomposition. *Journal of
    Computer and System Sciences*, 76(1), 3-20.
2.  **Belohlavek, R., & Trneckova, M. (2015).** Optimal decomposition of
    finite fuzzy relations: The problem and the GreConD algorithm.
    *Information Sciences*, 309, 133-157.
3.  **Miettinen, P., Mielikainen, T., Gionis, A., Das, G., & Mannila, H.
    (2008).** The discrete basis problem. *IEEE Transactions on
    Knowledge and Data Engineering*, 20(10), 1348-1362.
