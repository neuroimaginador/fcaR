# R6 class for a Bond Lattice

This class extends `ConceptLattice` to represent the lattice of bonds
between two formal contexts \\K_1\\ and \\K_2\\. It inherits all lattice
operations (finding irreducible elements, subconcepts, etc.) and
provides specific methods to extract bonds as matrices and
FormalContexts.

## Super classes

[`fcaR::ConceptSet`](https://neuroimaginador.github.io/fcaR/reference/ConceptSet.md)
-\>
[`fcaR::ConceptLattice`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.md)
-\> `BondLattice`

## Methods

### Public methods

- [`BondLattice$new()`](#method-BondLattice-new)

- [`BondLattice$get_bonds()`](#method-BondLattice-get_bonds)

- [`BondLattice$similarity()`](#method-BondLattice-similarity)

- [`BondLattice$get_core()`](#method-BondLattice-get_core)

- [`BondLattice$print()`](#method-BondLattice-print)

- [`BondLattice$is_bond()`](#method-BondLattice-is_bond)

- [`BondLattice$clone()`](#method-BondLattice-clone)

Inherited methods

- [`fcaR::ConceptSet$[()`](https://neuroimaginador.github.io/fcaR/reference/ConceptSet.html#method-%5B)
- [`fcaR::ConceptSet$extents()`](https://neuroimaginador.github.io/fcaR/reference/ConceptSet.html#method-extents)
- [`fcaR::ConceptSet$intents()`](https://neuroimaginador.github.io/fcaR/reference/ConceptSet.html#method-intents)
- [`fcaR::ConceptSet$is_empty()`](https://neuroimaginador.github.io/fcaR/reference/ConceptSet.html#method-is_empty)
- [`fcaR::ConceptSet$size()`](https://neuroimaginador.github.io/fcaR/reference/ConceptSet.html#method-size)
- [`fcaR::ConceptSet$sub()`](https://neuroimaginador.github.io/fcaR/reference/ConceptSet.html#method-sub)
- [`fcaR::ConceptSet$support()`](https://neuroimaginador.github.io/fcaR/reference/ConceptSet.html#method-support)
- [`fcaR::ConceptSet$to_latex()`](https://neuroimaginador.github.io/fcaR/reference/ConceptSet.html#method-to_latex)
- [`fcaR::ConceptSet$to_list()`](https://neuroimaginador.github.io/fcaR/reference/ConceptSet.html#method-to_list)
- [`fcaR::ConceptLattice$bottom()`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.html#method-bottom)
- [`fcaR::ConceptLattice$decompose()`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.html#method-decompose)
- [`fcaR::ConceptLattice$density()`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.html#method-density)
- [`fcaR::ConceptLattice$dimension()`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.html#method-dimension)
- [`fcaR::ConceptLattice$infimum()`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.html#method-infimum)
- [`fcaR::ConceptLattice$is_atomic()`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.html#method-is_atomic)
- [`fcaR::ConceptLattice$is_distributive()`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.html#method-is_distributive)
- [`fcaR::ConceptLattice$is_modular()`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.html#method-is_modular)
- [`fcaR::ConceptLattice$is_semimodular()`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.html#method-is_semimodular)
- [`fcaR::ConceptLattice$join_irreducibles()`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.html#method-join_irreducibles)
- [`fcaR::ConceptLattice$lower_neighbours()`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.html#method-lower_neighbours)
- [`fcaR::ConceptLattice$meet_irreducibles()`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.html#method-meet_irreducibles)
- [`fcaR::ConceptLattice$plot()`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.html#method-plot)
- [`fcaR::ConceptLattice$separation()`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.html#method-separation)
- [`fcaR::ConceptLattice$set_state()`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.html#method-set_state)
- [`fcaR::ConceptLattice$stability()`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.html#method-stability)
- [`fcaR::ConceptLattice$subconcepts()`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.html#method-subconcepts)
- [`fcaR::ConceptLattice$sublattice()`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.html#method-sublattice)
- [`fcaR::ConceptLattice$superconcepts()`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.html#method-superconcepts)
- [`fcaR::ConceptLattice$supremum()`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.html#method-supremum)
- [`fcaR::ConceptLattice$to_json()`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.html#method-to_json)
- [`fcaR::ConceptLattice$top()`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.html#method-top)
- [`fcaR::ConceptLattice$upper_neighbours()`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.html#method-upper_neighbours)
- [`fcaR::ConceptLattice$width()`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.html#method-width)

------------------------------------------------------------------------

### Method `new()`

Initialize a BondLattice object.

#### Usage

    BondLattice$new(extents, intents, objects, attributes, I, fc1, fc2)

#### Arguments

- `extents`:

  (dgCMatrix) The extents of all concepts

- `intents`:

  (dgCMatrix) The intents of all concepts

- `objects`:

  (character) Names of the objects (not required for bonds, can be
  generic)

- `attributes`:

  (character) Names of the attributes (flattened \$G_1 \times M_2\$)

- `I`:

  (matrix) Built incidence matrix

- `fc1`:

  (FormalContext) The first formal context

- `fc2`:

  (FormalContext) The second formal context

#### Returns

A new `BondLattice` object.

------------------------------------------------------------------------

### Method `get_bonds()`

Extract the bonds represented by the intents of the lattice.

#### Usage

    BondLattice$get_bonds(indices = NULL)

#### Arguments

- `indices`:

  (numeric or logical vector) The indices of the bonds (concepts) to
  extract. If `NULL` (default), extracts all.

#### Returns

A list of `FormalContext` objects, each representing one bond.

------------------------------------------------------------------------

### Method `similarity()`

Compute similarity, affinity, or complexity metrics between the two
contexts.

#### Usage

    BondLattice$similarity(
      type = c("log-bond", "top-density", "complexity", "core-agreement", "entropy",
        "stability", "width", "dimension", "width-index", "dimension-index")
    )

#### Arguments

- `type`:

  (character) The type of metric to compute:

  - `"log-bond"`: (Default) Normalized log-ratio of bonds. High value
    means high logical affinity.

  - `"top-density"`: Density of the largest possible bond (the top of
    the lattice).

  - `"complexity"`: Ratio of irreducible bonds to total bonds. Low value
    implies high structural emergence.

  - `"core-agreement"`: Ratio of filled cells in the Core bond versus
    the Top bond. Measures fundamental consensus.

  - `"entropy"`: Interaction entropy based on the log-size of the
    lattices.

  - `"stability"`: Average stability of the bonds in the lattice.

#### Returns

A numeric value representing the metric.

------------------------------------------------------------------------

### Method `get_core()`

Get the 'Core' bond (the smallest possible bond).

#### Usage

    BondLattice$get_core()

#### Returns

A `FormalContext` representing the minimal bond.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the BondLattice object.

#### Usage

    BondLattice$print()

#### Returns

Nothing, just prints the object summary.

------------------------------------------------------------------------

### Method [`is_bond()`](https://neuroimaginador.github.io/fcaR/reference/is_bond.md)

Verify if a relation is a bond between the internal contexts.

#### Usage

    BondLattice$is_bond(relation)

#### Arguments

- `relation`:

  (matrix or FormalContext) The relation to verify.

#### Returns

TRUE if it's a bond, FALSE otherwise.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    BondLattice$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

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

bl <- bonds(fc1, fc2)

# Extract all bonds as FormalContext objects
my_bonds <- bl$get_bonds()
print(my_bonds[[1]])
#> FormalContext with 5 objects and 3 attributes.
#>     B1  B2  B3  
#>  O1  X   X   X  
#>  O2  X   X   X  
#>  O3  X   X   X  
#>  O4  X   X   X  
#>  O5  X   X   X  
# }
```
