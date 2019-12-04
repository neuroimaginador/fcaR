## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(fcaR)

## -----------------------------------------------------------------------------
objects <- paste0("O", 1:6)
n_objects <- length(objects)

attributes <- paste0("P", 1:6)
n_attributes <- length(attributes)

I <- matrix(data = c(0, 1, 1, 0, 0, 1,
                     1, 1, 1, 0, 0, 0,
                     1, 1, 0, 0, 1, 0,
                     1, 0, 0, 1, 1, 0,
                     1, 0, 0, 1, 0, 0,
                     0, 0, 1, 0, 0, 0),
            nrow = n_objects,
            byrow = FALSE)

colnames(I) <- attributes
rownames(I) <- objects

## -----------------------------------------------------------------------------
fc <- FormalContext$new(I)

# Compute implications
fc$extract_implications_concepts(verbose = FALSE)

# Cardinality and mean size in the ruleset
fc$implications$cardinality()
sizes <- fc$implications$size()
colMeans(sizes)

## -----------------------------------------------------------------------------
fc$implications

## ----fig.width=7.5------------------------------------------------------------
# Visualize the concept lattice
fc$plot_lattice()
# And the formal context
fc$plot_context()

## -----------------------------------------------------------------------------
fc$implications$apply_rules(rules = c("composition",
                                      "generalization",
                                      "simplification"),
                            parallelize = FALSE)

# Compute cardinality and size in the transformed ruleset:
fc$implications$cardinality()
sizes <- fc$implications$size()
colMeans(sizes)

## -----------------------------------------------------------------------------
fc$implications

## -----------------------------------------------------------------------------
objects <- paste0("O", 1:6)
n_objects <- length(objects)

attributes <- paste0("P", 1:6)
n_attributes <- length(attributes)

I <- matrix(data = c(0, 1, 0.5, 0, 0, 0.5,
                     1, 1, 0.5, 0, 0, 0,
                     0.5, 1, 0, 0, 1, 0,
                     0.5, 0, 0, 1, 0.5, 0,
                     1, 0, 0, 0.5, 0, 0,
                     0, 0, 1, 0, 0, 0),
            nrow = n_objects,
            byrow = FALSE)

colnames(I) <- attributes
rownames(I) <- objects

## -----------------------------------------------------------------------------
fc <- FormalContext$new(I)

# Compute
fc$extract_implications_concepts(verbose = FALSE)

# Some properties of the ruleset
fc$implications$cardinality()
sizes <- fc$implications$size()
colMeans(sizes)

## -----------------------------------------------------------------------------
fc$implications

## ----fig.width=7.5------------------------------------------------------------
# Visualize the concept lattice
fc$plot_lattice()
# And the formal context
fc$plot_context()

## -----------------------------------------------------------------------------
fc$implications$apply_rules(rules = c("composition"), 
                            reorder = FALSE,
                            parallelize = FALSE)
fc$implications$cardinality()
sizes <- fc$implications$size()
colMeans(sizes)

fc$implications$apply_rules(rules = c("simplification"), 
                            reorder = FALSE,
                            parallelize = FALSE)
fc$implications$cardinality()
sizes <- fc$implications$size()
colMeans(sizes)

fc$implications$apply_rules(rules = c("generalization"), 
                            reorder = FALSE,
                            parallelize = FALSE)

fc$implications$cardinality()
sizes <- fc$implications$size()
colMeans(sizes)

## -----------------------------------------------------------------------------
fc$implications

## -----------------------------------------------------------------------------
S <- SparseSet$new(attributes = fc$attributes)
S$assign(attributes = c("P2", "P3"),
         values = c(0.5, 0.5))
S

fc$implications$compute_closure(S)

