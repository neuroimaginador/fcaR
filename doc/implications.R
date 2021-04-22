## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE
)

## ----setup--------------------------------------------------------------------
library(fcaR)

## -----------------------------------------------------------------------------
knitr::kable(planets, format = "html", booktabs = TRUE)

## ----echo = FALSE-------------------------------------------------------------
objects <- paste0("O", 1:6)
n_objects <- length(objects)

attributes <- paste0("P", 1:6)
n_attributes <- length(attributes)

I <- matrix(data = c(0, 1, 0.5, 0, 0, 0.5,
                     0, 1, 0.5, 0, 0, 0.5,
                     0.5, 1, 0, 0, 1, 0,
                     0.5, 0, 0, 1, 0.5, 0,
                     1, 0, 0, 0.5, 0, 0,
                     0, 0, 1, 0, 0, 1),
            nrow = n_objects,
            byrow = FALSE)

colnames(I) <- attributes
rownames(I) <- objects

## -----------------------------------------------------------------------------
knitr::kable(I, format = "html", booktabs = TRUE)

## -----------------------------------------------------------------------------
fc_planets <- FormalContext$new(planets)
fc_I <- FormalContext$new(I)

## -----------------------------------------------------------------------------
fc_planets$find_implications()
fc_I$find_implications()

## -----------------------------------------------------------------------------
fc_planets$implications
fc_I$implications

## -----------------------------------------------------------------------------
fc_planets$implications$get_LHS_matrix()
fc_planets$implications$get_RHS_matrix()

## -----------------------------------------------------------------------------
# Let us build a set of attributes
S <- Set$new(attributes = fc_planets$attributes)
S$assign(large = 1, far = 1)
S

fc_planets$implications$closure(S)$closure

## -----------------------------------------------------------------------------
# Let us clone the implication basis
imps <- fc_planets$implications$clone()
imps %holds_in% fc_planets

## -----------------------------------------------------------------------------
fc_planets %respects% imps

## -----------------------------------------------------------------------------
fc_planets$implications$cardinality()

## -----------------------------------------------------------------------------
sizes <- fc_planets$implications$size()
# Total number of attributes in the LHS and the RHS
colSums(sizes)

## -----------------------------------------------------------------------------
fc_planets$implications$support()

## -----------------------------------------------------------------------------
fc_planets$implications$to_latex()

## -----------------------------------------------------------------------------
# Implications with P1 and P2 in the LHS and P5 in the RHS
fc_I$implications$filter(lhs = c("P1", "P2"), 
                         rhs = "P5")

## -----------------------------------------------------------------------------
fc_I$implications$apply_rules(rules = c("composition",
                                        "simplification"))

## -----------------------------------------------------------------------------
# Let us build a set of attributes
S <- Set$new(attributes = fc_planets$attributes)
S$assign(large = 1, far = 1)
S

fc_planets$implications$closure(S, reduce = TRUE)

## -----------------------------------------------------------------------------
# imps is the basis
imps <- fc_planets$implications$clone()
imps2 <- imps$clone()
# imps2 is an equivalent set of implications
# where we have removed redundancies
imps2$apply_rules(c("simp", "rsimp"))
# Any implication in imps2 follows from imps
imps %entails% imps2
# And viceversa
imps2 %entails% imps

## -----------------------------------------------------------------------------
imps %~% imps2
# If we remove any implication from imps2,
# they will not be equivalent
imps %~% imps2[1:9]

## -----------------------------------------------------------------------------
S <- Set$new(attributes = fc_I$attributes)
S$assign(P1 = 1, P4 = 0.5)

fc_I$implications$recommend(S, attribute_filter = c("P3", "P5"))

