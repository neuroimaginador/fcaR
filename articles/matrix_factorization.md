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
algorithms:

- **RSF / RSF-ES:** *Rice-Siff Factorization* algorithms based on
  agglomerative clustering and a state-dependent semi-metric (Antoni et
  al., 2026). Rather than doing an exhaustive search of the attribute
  space, RSF guides factor selection by merging concepts dynamically.
  **RSF-ES** (Early Stopping) accelerates this by filtering out
  zero-coverage concepts and stopping when the distance reaches $`1.0`$
  (disjoint connected components), achieving up to a 10x speedup while
  preserving exactness.
- **GreConD:** *Greedy Concept on Demand* (Belohlavek & Vychodil, 2010).
  An exact BMF solver that greedily computes and selects formal concepts
  as factors to cover the remaining ones in the incidence relation.
- **ASSO:** A classic heuristic BMF algorithm based on association rules
  (Miettinen et al., 2008). It generates candidate factors using
  pairwise association confidence and is often fast, but it is
  approximate and allows overcovering (reconstructing a 1 where the
  original matrix had a 0).
- **GreEss:** A greedy BMF solver based on essential elements
  (Belohlavek & Trnecka, 2015). It restricts factor search strictly to
  mandatory submatrices (intervals) derived from essential entries of
  the context, producing a more parsimonious factorization.
- **PaNDa+:** A unifying framework for mining approximate top-$`k`$
  binary patterns based on the Minimum Description Length (MDL)
  principle (Lucchese et al., 2013). It extracts components that
  minimize data description complexity, adjusting to noise and pattern
  complexity.
- **Hyper / Hyper+:** Overlapped hyperrectangles summarization methods
  (Xiang et al., 2011). Hyper extracts closed patterns, and Hyper+
  optimizes the representation cost under a false positive error budget.
  If the error budget is set to zero, it works as an exact BMF solver.

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

- **Factor 1 (`F1`)** is present in `Labrador` and `Golden Ret.`.
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

- **Factor 1 (`F1`)** is characterized by the attributes `Friendly` and
  `Playful`.
- **Factor 2 (`F2`)** is characterized by the attributes `Guard` and
  `Aggressive`.
- **Factor 3 (`F3`)** is characterized by the attributes `Friendly` and
  `Small`.

Combining these observations: 1. **Factor 1 (`F1`)** represents the
archetype of a **“Friendly and Playful Dog”** (exhibited by Labradors
and Golden Retrievers). 2. **Factor 2 (`F2`)** represents the archetype
of a **“Guard/Protective Dog”** (exhibited by German Shepherds and
Rottweilers). 3. **Factor 3 (`F3`)** represents the archetype of a
**“Small and Friendly Dog”** (exhibited by Chihuahuas).

------------------------------------------------------------------------

## 2. Rice-Siff Factorization (RSF and RSF-ES)

The **Rice-Siff Factorization (RSF)** family represents a hybrid BMF
approach. Instead of performing an exhaustive combinatorial sweep of the
attribute space (like GreConD), it guides the greedy selection of
factors using a hierarchical agglomerative clustering method derived
from the Rice-Siff algorithm.

The core of the RSF algorithm relies on a state-dependent semi-metric
$`\rho_{un}`$, which quantifies the distance between candidate concepts
based on the currently uncovered cells in the residual relation
$`R_{un}`$:
``` math
 \rho_{un}((X_1, Y_1), (X_2, Y_2)) = 1 - \frac{|(X_1 \times Y_1) \cap (X_2 \times Y_2) \cap R_{un}|}{|(X_1 \times Y_1) \cup (X_2 \times Y_2)|} 
```

By prioritizing merges that overlap on remaining uncovered cells, the
agglomeration natively converges to formal concepts that maximize the
coverage of the residual matrix.

### RSF with Early Stopping (RSF-ES)

To prevent the combinatorial explosion in high-dimensional contexts,
**RSF-ES** introduces two key optimizations: 1. **Zero-weight
Filtering:** Candidates whose generating components have no coverage on
$`R_{un}`$ ($`w = 0`$) are pruned early. 2. **Topological Pruning:** The
agglomerative loop is immediately broken when the minimum distance
reaches $`\rho_{un} = 1.0`$. In graph-theoretic terms, a distance of
$`1.0`$ indicates that the candidates belong to disjoint connected
components in the residual relation. Fusing them would restrict the
resulting formal intent without yielding any structural gain, making
further merging redundant.

These optimizations allow RSF-ES to achieve up to a **10x speedup** on
dense or high-dimensional matrices compared to standard RSF while
maintaining mathematical exactness.

### Performing RSF Factorization

You can use both `RSF` and `RSF-ES` directly inside the `factorize`
method:

``` r

# Factorize using RSF
res_rsf <- fc$factorize(method = "RSF")
print(res_rsf$factor_attribute$incidence())
#>    Friendly Playful Guard Aggressive Small
#> F1        1       1     0          0     0
#> F2        0       0     1          1     0
#> F3        1       0     0          0     1

# Factorize using RSF-ES (Highly optimized)
res_rsfes <- fc$factorize(method = "RSF-ES")
print(res_rsfes$factor_attribute$incidence())
#>    Friendly Playful Guard Aggressive Small
#> F1        1       1     0          0     0
#> F2        0       0     1          1     0
#> F3        1       0     0          0     1
```

------------------------------------------------------------------------

## 3. Other Algorithms: ASSO

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

## Acknowledgments

The authors are grateful to **Dr. Martin Trnecka**, from Palacký
University Olomouc, for his generosity in sharing the original source
code and granting permission for its porting to R and C/C++. This
contribution was pivotal in enabling the high-performance implementation
of these algorithms and the subsequent rigorous comparative analysis.

------------------------------------------------------------------------

## References

1.  **Antoni, L., Kotlárová, D., Krídlo, O., López-Rodríguez, D., &
    Ojeda-Aciego, M. (2026).** Effective greedy Boolean matrix
    factorization via the Rice-Siff algorithm. *International Journal of
    Approximate Reasoning*, 197, 109747.
2.  **Belohlavek, R. (2010).** Discovery of optimal factors in binary
    data via a novel method of matrix decomposition. *Journal of
    Computer and System Sciences*, 76(1), 3-20.
3.  **Belohlavek, R., & Trneckova, M. (2015).** Optimal decomposition of
    finite fuzzy relations: The problem and the GreConD algorithm.
    *Information Sciences*, 309, 133-157.
4.  **Belohlavek, R., & Trnecka, M. (2015).** From-below approximations
    in Boolean matrix factorization: Geometry and new algorithm.
    *Journal of Computer and System Sciences*, 81(8), 1678-1714.
5.  **Lucchese, C., Orlando, S., & Perego, R. (2013).** A unifying
    framework for mining approximate top-k binary patterns. *IEEE
    Transactions on Knowledge and Data Engineering*, 26(12), 2900-2913.
6.  **Miettinen, P., Mielikainen, T., Gionis, A., Das, G., & Mannila, H.
    (2008).** The discrete basis problem. *IEEE Transactions on
    Knowledge and Data Engineering*, 20(10), 1348-1362.
7.  **Xiang, Y., Jin, R., Fuhry, D., & Dragan, F. F. (2011).**
    Summarizing transactional databases with overlapped hyperrectangles.
    *Data Mining and Knowledge Discovery*, 23(2), 215-251.
