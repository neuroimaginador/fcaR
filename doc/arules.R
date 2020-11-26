## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE
)

## ----setup--------------------------------------------------------------------
library(arules)
library(fcaR)

## -----------------------------------------------------------------------------
data("Mushroom", package = "arules")

## -----------------------------------------------------------------------------
fc_planets <- FormalContext$new(planets)

## -----------------------------------------------------------------------------
fc <- FormalContext$new(Mushroom)
fc

## -----------------------------------------------------------------------------
fc_planets$to_transactions()

## -----------------------------------------------------------------------------
mushroom_rules <- apriori(Mushroom, parameter = list(conf = 1), control = list(verbose = FALSE))

## -----------------------------------------------------------------------------
fc$implications$add(mushroom_rules)

## -----------------------------------------------------------------------------
fc_planets$find_implications()
fc_planets$implications$to_arules(quality = TRUE)

