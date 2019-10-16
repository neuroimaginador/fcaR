context("ImplicationSet")

library(arules)

data("Mushroom", package = "arules")
expect_warning(mush <- apriori(Mushroom, parameter = list(conf = 1, maxlen = 4)))

idx_redundant <- is.redundant(mush)

mush_clean <- mush[!idx_redundant]

test_that("fcaR operates on implications", {

  fc <- formal_context$new(I = Mushroom)

  fc$add_implications(mush_clean)

  # Cadinality
  expect_is(fc$implications$cardinality(), "integer")

  # Rule size
  expect_is(fc$implications$size(), "matrix")

  # Use composition to reduce the number of implications
  expect_error(fc$implications$apply_rules(rules = c("composition")), NA)
  expect_is(fc$implications, "ImplicationSet")

  # Simplification
  expect_error(fc$implications$apply_rules(rules = c("simplification")), NA)
  expect_is(fc$implications, "ImplicationSet")

  # At this moment, we're at a fixed point, but we could apply
  # some more rules if needed:
  expect_error(fc$implications$apply_rules(rules = c("generalization",
                                                     "composition",
                                                     "simplification",
                                                     "reduction"),
                                           reorder = TRUE), NA)
  expect_is(fc$implications, "ImplicationSet")

})

test_that("fcaR prints implications", {

  fc <- formal_context$new(I = Mushroom)

  fc$add_implications(mush_clean)

  expect_error(fc$implications, NA)

})

test_that("fcaR adds and appends implications", {

  fc <- formal_context$new(I = Mushroom)

  fc$add_implications(mush_clean)

  fc$implications$append_implications(fc$implications)
  first_lhs <- .extract_column(fc$implications$get_LHS_matrix(), 1)
  first_rhs <- .extract_column(fc$implications$get_RHS_matrix(), 1)

  expect_error(fc$implications$add_implication(lhs = first_lhs, rhs = first_rhs), NA)

})

test_that("fcaR exports implications to latex", {

  fc <- formal_context$new(I = Mushroom)

  fc$add_implications(mush_clean)

  expect_error(fc$implications$get_rules(1:10)$to_latex(), NA)

})

test_that("fcaR gets LHS and RHS of implications", {

  fc <- formal_context$new(I = Mushroom)

  fc$implications <- implication_set$new(attributes = fc$attributes)

  expect_is(fc$implications$get_LHS_matrix(), "lgCMatrix")
  expect_is(fc$implications$get_RHS_matrix(), "lgCMatrix")

  fc$add_implications(mush_clean)

  expect_is(fc$implications$get_LHS_matrix(), "dgCMatrix")
  expect_is(fc$implications$get_RHS_matrix(), "dgCMatrix")

})

test_that("fcaR computes closure wrt implications", {

  fc <- formal_context$new(I = Mushroom)

  fc$add_implications(mush_clean)

  # A fuzzy set
  A <- sparse_set$new(attributes = fc$attributes)
  A$assign(attributes = "CapColor=white", values = 1)

  # Compute the closure
  expect_error(cl <- fc$implications$compute_closure(A, reduce = TRUE, verbose = TRUE), NA)
  # Associated attributes
  expect_is(cl$closure, "SparseSet")

  expect_is(cl$implications, "ImplicationSet")

})

test_that("fcaR simplifies implications", {

  fc <- formal_context$new(I = Mushroom)

  fc$add_implications(mush_clean)

  L <- .simplification(LHS = fc$implications$get_LHS_matrix(),
                       RHS = fc$implications$get_RHS_matrix(),
                       attributes = fc$attributes,
                       trace = TRUE)

  expect_is(L, "list")

})

test_that("fcaR makes a recommendation", {

  fc <- formal_context$new(I = Mushroom)

  fc$add_implications(mush_clean)

  # A fuzzy set
  S <- sparse_set$new(attributes = fc$attributes)
  S$assign(attributes = "CapColor=white", values = 1)

  expect_error(fc$implications$recommend(S = S, attribute_filter = fc$attributes[1]), NA)

})

test_that("fcaR filters and removes implications", {

  fc <- formal_context$new(I = Mushroom)

  fc$add_implications(mush_clean)

  expect_error(fc$implications$filter_by_lhs(attr_filter = fc$attributes[1]), NA)
  expect_error(fc$implications$filter_by_lhs(attr_filter = fc$attributes[1:2]), NA)

  expect_error(fc$implications$filter_by_rhs(attr_filter = fc$attributes[1]), NA)
  expect_error(fc$implications$filter_by_rhs(attr_filter = fc$attributes[1:2]), NA)
  expect_error(fc$implications$filter_by_rhs(attr_filter = fc$attributes[1],
                                             drop = TRUE), NA)

  n <- fc$implications$cardinality()

  expect_error(fc$implications$remove_rules(1:2), NA)

  n2 <- fc$implications$cardinality()

  expect_equal(n2, n - 2)

})

test_that("fcaR adds implications from scratch", {

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

  fc$implications <- implication_set$new(attributes = fc$attributes)
  expect_equal(fc$implications$cardinality(), 0)

  lhs1 <- sparse_set$new(attributes = fc$attributes)
  lhs1$assign(attributes = fc$attributes[1],
              values = 1)

  rhs1 <- sparse_set$new(attributes = fc$attributes)
  rhs1$assign(fc$attributes[c(2,4)],
              values = c(1, 1))

  expect_error(fc$implications$add_implication(lhs = lhs1, rhs = rhs1), NA)

})

test_that("fcaR can use generalization", {

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

  fc$implications <- implication_set$new(attributes = fc$attributes)
  expect_equal(fc$implications$cardinality(), 0)

  lhs1 <- sparse_set$new(attributes = fc$attributes)
  lhs1$assign(attributes = fc$attributes[1],
              values = 1)

  rhs1 <- sparse_set$new(attributes = fc$attributes)
  rhs1$assign(fc$attributes[c(2,4)],
              values = c(1, 1))

  fc$implications$add_implication(lhs = lhs1, rhs = rhs1)

  lhs2 <- sparse_set$new(attributes = fc$attributes)
  lhs2$assign(attributes = fc$attributes[c(1, 3)],
              values = c(1, 1))

  rhs2 <- sparse_set$new(attributes = fc$attributes)
  rhs2$assign(fc$attributes[4],
              values = 1)

  fc$implications$add_implication(lhs = lhs2, rhs = rhs2)

  expect_error(fc$implications$apply_rules(rules = "generalization", parallelize = FALSE), NA)

})

test_that("fcaR filters implications", {

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

  fc$implications <- implication_set$new(attributes = fc$attributes)
  expect_equal(fc$implications$cardinality(), 0)

  lhs1 <- sparse_set$new(attributes = fc$attributes)
  lhs1$assign(attributes = fc$attributes[1],
              values = 1)

  rhs1 <- sparse_set$new(attributes = fc$attributes)
  rhs1$assign(fc$attributes[c(2,4)],
              values = c(1, 1))

  fc$implications$add_implication(lhs = lhs1, rhs = rhs1)

  expect_warning(fc$implications$filter_by_lhs(attr_filter = fc$attributes[5]))
  expect_warning(fc$implications$filter_by_rhs(attr_filter = fc$attributes[5]))

})
