# Matrix Factorization with GreConD+

``` r
library(fcaR)
```

## Introduction

Formal Concept Analysis (FCA) usually deals with the complete lattice of
concepts. However, in many applications, we are interested in a smaller
set of “fundamental” concepts that explain the data efficiently. This is
known as **Boolean (or Fuzzy) Matrix Factorization**.

Given an object-attribute matrix $I$ (size $n \times m$), we look for
two smaller matrices:

1.  **Object-Factor matrix ($A$)** ($n \times k$): To what degree does
    each object belong to a factor?
2.  **Factor-Attribute matrix ($B$)** ($k \times m$): To what degree is
    each attribute a manifestation of a factor?

The goal is that the composition of these matrices approximates the
original data: $I \approx A \circ B$.

`fcaR` implements the **GreConD+** algorithm (Belohlavek & Trneckova,
2024), which is designed to find a minimal set of factors allowing for
slight “overcovering” (covering more than the original data implies) to
drastically reduce the number of factors needed while maintaining high
precision.

## Example: Fuzzy Dog Breeds

We will use a simplified fuzzy dataset describing dog breeds and their
characteristics. The values range from 0 (doesn’t apply) to 1 (applies
fully), with intermediate values like 0.5 (applies somewhat).

### 1. Defining the Context

``` r
# A fuzzy matrix representing dog breeds
data_matrix <- matrix(c(
  # Labr, Goldn, York, G.Shep, Beagl
    0.8,   0.6,   0.8,   0.6,   0.6,  # Energy
    1.0,   1.0,   0.8,   0.4,   0.6,  # Playfulness
    0.8,   1.0,   0.4,   0.2,   1.0,  # Friendliness towards dogs
    1.0,   1.0,   0.6,   0.4,   1.0,  # Friendliness towards strangers
    0.4,   0.4,   0.2,   1.0,   0.2,  # Protection ability
    0.6,   0.6,   0.2,   0.8,   0.6,  # Exercise needs
    1.0,   1.0,   0.6,   0.6,   1.0,  # Affection
    1.0,   1.0,   0.4,   1.0,   0.2   # Ease of training
), nrow = 5, byrow = FALSE) # Transposed to match objects as rows

colnames(data_matrix) <- c("Energy", "Playfulness", "Friend. Dogs", 
                           "Friend. Strangers", "Protection", "Exercise", 
                           "Affection", "Training")
rownames(data_matrix) <- c("Labrador", "Golden Ret.", "Yorkshire", 
                           "German Shep.", "Beagle")

# Create the FormalContext
fc <- FormalContext$new(data_matrix)

# Inspect the raw data
print(fc)
#> FormalContext with 5 objects and 8 attributes.
#>               Energy  Playfulness  Friend. Dogs  Friend. Strangers  Protection  
#>      Labrador   0.8        1            0.8              1              0.4     
#>   Golden Ret.   0.6        1             1               1              0.4     
#>     Yorkshire   0.8       0.8           0.4             0.6             0.2     
#>  German Shep.   0.6       0.4           0.2             0.4              1      
#>        Beagle   0.6       0.6            1               1              0.2     
#> Other attributes are: Exercise, Affection, Training
```

### 2. Applying Factorization

We use the `factorize()` method.

- `w`: Weight for the overcovering penalty. A higher `w` punishes
  overcovering more (leading to more precise, but potentially more
  numerous factors). `w=0.5` is a balanced choice.

``` r
# Run GreConD+
factors <- fc$factorize(w = 0.5)
```

The result is a list containing two new `FormalContext` objects
corresponding to matrices $A$ and $B$.

### 3. Interpreting the Factors

The algorithm has automatically discovered latent concepts (factors) in
our data. Let’s analyze them based on the `factor_attribute` matrix.

#### The Meaning of Factors (Matrix B)

The `factor_attribute` context ($B$) tells us **what** each factor
represents in terms of attributes.

``` r
# Matrix B: Factors x Attributes
factors$factor_attribute$print()
#> FormalContext with 3 objects and 8 attributes.
#>     Energy  Playfulness  Friend. Dogs  Friend. Strangers  Protection  Exercise  
#>  F1   0.6        1             1               1              0.2        0.6    
#>  F2   0.6       0.4           0.2             0.4              1         0.8    
#>  F3   0.8       0.8           0.4             0.6             0.2        0.2    
#> Other attributes are: Affection, Training
```

- **Factor 1 (F1) - ” The Ideal Companion”:** This factor exhibits
  maximal degrees (1.0) in **Playfulness**, **Friendliness** (towards
  dogs and strangers), **Affection**, and **Ease of Training**. It
  represents the archetype of a perfect family dog: highly sociable,
  trainable, and affectionate, with low requirement for protection
  duties (0.2).

- **Factor 2 (F2) - “The Guardian”:** This factor is characterized by a
  maximal degree (1.0) in **Protection Ability** and **Ease of
  Training**, along with a high demand for **Exercise** (0.8).
  Conversely, it shows low scores in friendliness towards other dogs
  (0.2) and playfulness (0.4). This clearly describes a working or guard
  dog profile.

- **Factor 3 (F3) - “The Energetic Terrier”:** This factor stands out
  for high **Energy** (0.8) and **Playfulness** (0.8), but has lower
  scores in protection and training compared to the others.
  Interestingly, this factor corresponds closely to the specific profile
  of smaller, energetic breeds (like the Yorkshire Terrier in our
  dataset) that don’t fit perfectly into the pure “Guardian” or
  “Universal Companion” categories.

#### The Objects in Factors (Matrix A)

The `object_factor` context ($A$) tells us **which dogs** possess these
factors.

``` r
# Matrix A: Objects x Factors
factors$object_factor$print()
#> FormalContext with 5 objects and 3 attributes.
#>                F1   F2   F3  
#>      Labrador  1   0.4   1   
#>   Golden Ret.  1   0.4  0.6  
#>     Yorkshire 0.2  0.2   1   
#>  German Shep. 0.2   1   0.2  
#>        Beagle  1   0.2  0.2
```

*Interpretation based on the factors above:*

- **Labrador & Golden Retriever:** We expect them to have very high
  degrees in **F1** (Companion).
- **German Shepherd:** Should have a dominant degree in **F2**
  (Guardian).
- **Yorkshire Terrier:** Will likely define **F3**.
- **Beagle:** Might show a mix, primarily **F1**.

### 4. Verifying the Decomposition

We can visualize how good the approximation is by reconstructing
$I\prime = A \circ B$. Since GreConD+ aims for minimal error, the
reconstruction should be very close to the original.

``` r
# Extract matrices
A <- factors$object_factor$incidence()
B <- factors$factor_attribute$incidence()

# Use Lukasiewicz product: (A o B)_ij = Max_k (A_ik + B_kj - 1, 0)
reconstructed_I <- matrix(0, nrow = nrow(A), ncol = ncol(B))

for (i in 1:nrow(A)) {
  for (j in 1:ncol(B)) {
    # Vectorized fuzzy composition
    vals <- pmax(0, A[i, ] + B[, j] - 1)
    reconstructed_I[i, j] <- max(vals)
  }
}

# Compare with original
print("Reconstructed Matrix:")
#> [1] "Reconstructed Matrix:"
print(reconstructed_I)
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#> [1,]  0.8  1.0  1.0  1.0  0.4  0.6  1.0  1.0
#> [2,]  0.6  1.0  1.0  1.0  0.4  0.6  1.0  1.0
#> [3,]  0.8  0.8  0.4  0.6  0.2  0.2  0.6  0.4
#> [4,]  0.6  0.4  0.2  0.4  1.0  0.8  0.6  1.0
#> [5,]  0.6  1.0  1.0  1.0  0.2  0.6  1.0  1.0

print("Original Matrix:")
#> [1] "Original Matrix:"
print(fc$incidence())
#>              Energy Playfulness Friend. Dogs Friend. Strangers Protection
#> Labrador        0.8         1.0          0.8               1.0        0.4
#> Golden Ret.     0.6         1.0          1.0               1.0        0.4
#> Yorkshire       0.8         0.8          0.4               0.6        0.2
#> German Shep.    0.6         0.4          0.2               0.4        1.0
#> Beagle          0.6         0.6          1.0               1.0        0.2
#>              Exercise Affection Training
#> Labrador          0.6       1.0      1.0
#> Golden Ret.       0.6       1.0      1.0
#> Yorkshire         0.2       0.6      0.4
#> German Shep.      0.8       0.6      1.0
#> Beagle            0.6       1.0      0.2

# Calculate Mean Absolute Error
mae <- mean(abs(fc$incidence() - reconstructed_I))
print(paste("Mean Absolute Error:", mae))
#> [1] "Mean Absolute Error: 0.035"
```

### Conclusion

Matrix factorization in `fcaR` allows us to reduce complex fuzzy
datasets into a few meaningful factors. Unlike statistical methods like
PCA, the factors in FCA are **interpretable concepts** (subsets of
attributes) that are subsets of the original data logic, preserving the
semantic structure of the information.
