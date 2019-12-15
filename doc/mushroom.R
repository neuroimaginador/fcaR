## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(fcaR)

## -----------------------------------------------------------------------------
library(arules)

data("Mushroom", package = "arules")

mush <- apriori(Mushroom, parameter = list(conf = 1))

## -----------------------------------------------------------------------------
system.time(
  idx_redundant <- is.redundant(mush)
)

mush_clean <- mush[!idx_redundant]

## -----------------------------------------------------------------------------
fc <- FormalContext$new(I = Mushroom)

fc$implications$add(mush_clean)

## -----------------------------------------------------------------------------
# Cadinality
fc$implications$cardinality()

# Rule size
sizes <- fc$implications$size()

# Mean size for LHS and RHS
colMeans(sizes)

## -----------------------------------------------------------------------------
# Use composition to reduce the number of implications
fc$implications$apply_rules(rules = c("composition"),
                            parallelize = FALSE)

fc$implications$cardinality()
sizes <- fc$implications$size()
colMeans(sizes)

# Simplification
fc$implications$apply_rules(rules = c("simplification"),
                            parallelize = FALSE)

fc$implications$cardinality()
sizes <- fc$implications$size()
colMeans(sizes)

# At this moment, we're at a fixed point, but we could apply
# some more rules if needed:
fc$implications$apply_rules(rules = c("composition",
                                      "generalization",
                                      "simplification"),
                            parallelize = FALSE)

## -----------------------------------------------------------------------------
supp <- fc$implications$support()

head(supp)

## -----------------------------------------------------------------------------
# A fuzzy set
A <- SparseSet$new(attributes = fc$attributes)
A$assign(attributes = "CapColor=white", values = 1)

# Compute the closure
fc$implications$closure(A)


## -----------------------------------------------------------------------------
R <- fc$implications$to_arules()

R

class(R)

