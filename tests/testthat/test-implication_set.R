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
  expect_error(fc$implications$apply_rules(rules = c("composition",
                                                     "generalization",
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
  first_lhs <- extract_column_sparse(fc$implications$get_LHS_matrix(), 1)
  first_rhs <- extract_column_sparse(fc$implications$get_RHS_matrix(), 1)

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

  # LHS of the fifth rule
  A <- fc$implications$get_LHS_matrix()[, 5]
  # Associated attributes
  A <- build_set(fc$attributes, A, fc$attributes)

  # Compute the closure
  expect_error(cl <- fc$implications$compute_closure(A, reduce = TRUE, verbose = TRUE), NA)
  # Associated attributes
  print_set(cl$closure, fc$attributes)

  new_impls <- implication_set$new(lhs = cl$implications$lhs,
                                   rhs = cl$implications$rhs,
                                   name = "reduced")

  expect_is(new_impls, "ImplicationSet")

})

test_that("fcaR simplifies implications", {

  fc <- formal_context$new(I = Mushroom)

  fc$add_implications(mush_clean)

  L <- .simplify2_lhs_rhs(LHS = fc$implications$get_LHS_matrix(),
                          RHS = fc$implications$get_RHS_matrix(),
                          attributes = fc$attributes,
                          trace = FALSE)

  expect_is(L, "list")

})

test_that("fcaR makes a recommendation", {

  fc <- formal_context$new(I = Mushroom)

  fc$add_implications(mush_clean)

  # LHS of the fifth rule
  S <- build_set(fc$attributes, fc$implications$get_LHS_matrix()[, 5], fc$attributes)
  expect_error(fc$implications$recommend(S = S, attribute_filter = fc$attributes[1]), NA)

})

test_that("fcaR filters and removes implications", {

  fc <- formal_context$new(I = Mushroom)

  fc$add_implications(mush_clean)

  expect_error(fc$implications$filter_by_lhs(attr_filter = fc$attributes[1]), NA)

  expect_error(fc$implications$filter_by_rhs(attr_filter = fc$attributes[1]), NA)
  expect_error(fc$implications$filter_by_rhs(attr_filter = fc$attributes[1],
                                drop = TRUE), NA)

  n <- fc$implications$cardinality()

  expect_error(fc$implications$remove_rules(1:2), NA)

  n2 <- fc$implications$cardinality()

  expect_equal(n2, n - 2)

})
