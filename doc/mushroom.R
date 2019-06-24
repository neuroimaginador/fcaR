## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(fcaR)

## ------------------------------------------------------------------------
library(arules)

data("Mushroom", package = "arules")

mush <- apriori(Mushroom, parameter = list(conf = 1))

## ------------------------------------------------------------------------
system.time(
  idx_redundant <- is.redundant(mush)
)

mush_clean <- mush[!idx_redundant]

## ------------------------------------------------------------------------
fc <- formal_context$new(I = Mushroom)

fc$add_implications(mush_clean)

## ------------------------------------------------------------------------
# Cadinality
fc$implications$cardinality()

# Rule size
sizes <- fc$implications$size()

# Mean size for LHS and RHS
colMeans(sizes)

## ------------------------------------------------------------------------
# Use composition to reduce the number of implications
fc$implications$apply_rules(rules = c("composition"))

fc$implications$cardinality()
sizes <- fc$implications$size()
colMeans(sizes)

# Simplification
fc$implications$apply_rules(rules = c("simplification"))

fc$implications$cardinality()
sizes <- fc$implications$size()
colMeans(sizes)

# At this moment, we're at a fixed point, but we could apply
# some more rules if needed:
fc$implications$apply_rules(rules = c("composition",
                                      "generalization",
                                      "simplification"))

## ------------------------------------------------------------------------
supp <- fc$get_implication_support()

head(supp)

## ------------------------------------------------------------------------
# LHS of the fifth rule
A <- fc$implications$get_LHS_matrix()[, 5]
# Associated attributes
print_set(A, fc$attributes)

# Compute the closure
cl <- fc$implications$compute_closure(A)
# Associated attributes
print_set(cl, fc$attributes)

## ------------------------------------------------------------------------
R <- fc$convert_implications_to_arules()

R

class(R)

