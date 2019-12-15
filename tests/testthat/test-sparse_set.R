context("SparseSet")

test_that("fcaR operates on sparse sets", {

  A <- matrix(1, nrow = 10, ncol = 1)
  B <- matrix(1, nrow = 10, ncol = 5)

  expect_error(C <- .difference(A, B), NA)
  expect_error(C <- .difference(B, B), NA)
  expect_error(C <- .difference(B, A), NA)

  expect_error(C <- .union(A, B), NA)
  expect_error(C <- .union(B, B), NA)
  expect_error(C <- .union(B, A), NA)

})

test_that("fcaR operates on sparse sets II", {

  A <- Matrix(c(FALSE, TRUE, FALSE), nrow = 3, ncol = 1)
  B <- matrix(1, nrow = 3, ncol = 5)

  expect_error(C <- .difference(A, B), NA)
  expect_error(C <- .difference(B, B), NA)
  expect_error(C <- .difference(B, A), NA)

  expect_error(C <- .union(A, B), NA)
  expect_error(C <- .union(B, B), NA)
  expect_error(C <- .union(B, A), NA)

})

test_that("fcaR uses class SparseSet", {

  attributes <- paste0("P", 1:6)

  expect_error(A <- SparseSet$new(attributes = attributes), NA)
  expect_error(A$assign(attributes = "P1", values = 0.3), NA)

  expect_is(A$get_vector(), "Matrix")

  expect_equal(A$get_attributes(), attributes)

  expect_equal(A$length(), 6)

  expect_output(A$print())

  expect_is(A$to_latex(), "character")

  expect_true(A %<=% A)

})
