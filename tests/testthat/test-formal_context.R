context("Formal Context")

library(arules)

data("Mushroom", package = "arules")
expect_warning(mush <- apriori(Mushroom, parameter = list(conf = 1, maxlen = 4)))

idx_redundant <- is.redundant(mush)

mush_clean <- mush[!idx_redundant]

test_that("fcaR creates a formal context", {

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

  fc <- formal_context$new(I = I)

  expect_is(fc, "FormalContext")

})

test_that("fcaR extracts concepts", {

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

  fc <- formal_context$new(I = I)

  concepts <- fc$compute_concepts()

  expect_is(concepts, "list")

})

test_that("fcaR extracts implications", {

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

  fc <- formal_context$new(I = I)

  fc$extract_implications_concepts()

  expect_is(fc$implications, "ImplicationSet")

})

test_that("fcaR generate plots", {

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

  fc <- formal_context$new(I = I)

  fc$extract_implications_concepts()

  expect_error(fc$plot_context(), NA)
  expect_error(fc$plot_lattice(), NA)

})

test_that("fcaR imports formal contexts from arules", {

  fc <- formal_context$new(I = Mushroom)

  expect_is(fc, "FormalContext")

})

test_that("fcaR imports implications from arules", {

  fc <- formal_context$new(I = Mushroom)
  fc$add_implications(mush_clean)

  expect_is(fc$implications, "ImplicationSet")

})


test_that("fcaR exports formal contexts to arules transactions", {

  fc <- formal_context$new(I = Mushroom)

  expect_is(fc$convert_to_transactions(), "transactions")

})

test_that("fcaR exports implications to arules", {


  fc <- formal_context$new(I = Mushroom)

  fc$add_implications(mush_clean)

  fc$implications$apply_rules("composition")

  my_rules <- fc$export_implications_to_arules(quality = TRUE)

  expect_is(my_rules, "rules")

})

test_that("fcaR computes concept support", {

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

  fc <- formal_context$new(I = I)

  fc$extract_implications_concepts()

  expect_error(fc$get_concept_support(), NA)


})

test_that("fcaR computes implication support", {

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

  fc <- formal_context$new(I = I)

  fc$extract_implications_concepts()

  expect_error(fc$get_implication_support(), NA)

})
