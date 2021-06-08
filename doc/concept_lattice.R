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
print(fc_planets)
print(fc_I)

## ----fig.dim=c(4,4)-----------------------------------------------------------
fc_planets$plot()
fc_I$plot()

## -----------------------------------------------------------------------------
fc_planets$to_latex()

## -----------------------------------------------------------------------------
# Read CSV
filename <- system.file("contexts", "airlines.csv",
                        package = "fcaR")

fc1 <- FormalContext$new(filename)
fc1

# Read CXT
filename <- system.file("contexts", "lives_in_water.cxt",
                        package = "fcaR")

fc2 <- FormalContext$new(filename)
fc2

## -----------------------------------------------------------------------------
fc_dual <- fc_planets$dual()
fc_dual

## -----------------------------------------------------------------------------
# Define a set of objects
S <- Set$new(attributes = fc_planets$objects)
S$assign(Earth = 1, Mars = 1)
S

# Compute the intent of S
fc_planets$intent(S)

## -----------------------------------------------------------------------------
# Define a set of objects
S <- Set$new(attributes = fc_planets$attributes)
S$assign(moon = 1, large = 1)
S

# Compute the extent of S
fc_planets$extent(S)

## -----------------------------------------------------------------------------
# Compute the closure of S
Sc <- fc_planets$closure(S)
Sc

## -----------------------------------------------------------------------------
fc_planets$is_closed(S)
fc_planets$is_closed(Sc)

## -----------------------------------------------------------------------------
fc_planets$reduce(TRUE)

fc_I$clarify(TRUE)

## -----------------------------------------------------------------------------
fc_planets$find_implications()

fc_I$find_implications()

## -----------------------------------------------------------------------------
# Concepts
fc_planets$concepts

# Implications
fc_planets$implications

## -----------------------------------------------------------------------------
fc_planets$standardize()
fc_I$standardize()

## ----eval = FALSE-------------------------------------------------------------
#  fc$save(filename = "./fc.rds")

## ----eval = FALSE-------------------------------------------------------------
#  fc2 <- FormalContext$new("./fc.rds")

## -----------------------------------------------------------------------------
fc_planets$concepts$plot()
fc_I$concepts$plot()

## -----------------------------------------------------------------------------
# Printing
fc_planets$concepts

# LaTeX
fc_planets$concepts$to_latex()

## -----------------------------------------------------------------------------
fc_planets$concepts[2:3]

## ----eval = FALSE-------------------------------------------------------------
#  fc_planets$concepts$extents()
#  fc_planets$concepts$intents()

## -----------------------------------------------------------------------------
fc_planets$concepts$support()

## -----------------------------------------------------------------------------
# Get the index of those concepts with support 
# greater than the threshold
idx <- which(fc_I$concepts$support() > 0.2)
# Build the sublattice
sublattice <- fc_I$concepts$sublattice(idx)
sublattice

## -----------------------------------------------------------------------------
sublattice$plot()

## -----------------------------------------------------------------------------
# The fifth concept
C <- fc_planets$concepts$sub(5)
C
# Its subconcepts:
fc_planets$concepts$subconcepts(C)
# And its superconcepts:
fc_planets$concepts$superconcepts(C)

## -----------------------------------------------------------------------------
# A list of concepts
C <- fc_planets$concepts[5:7]
C

# Supremum of the concepts in C
fc_planets$concepts$supremum(C)
# Infimum of the concepts in C
fc_planets$concepts$infimum(C)

## -----------------------------------------------------------------------------
fc_planets$concepts$join_irreducibles()
fc_planets$concepts$meet_irreducibles()

