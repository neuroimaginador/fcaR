context("SparseSet")

test_that("fcaR operates on sparse sets", {

  A <- matrix(1, nrow = 10, ncol = 1)
  B <- matrix(1, nrow = 10, ncol = 5)

  expect_error(C <- sparse_set_difference(A, B), NA)
  expect_error(C <- sparse_set_difference(B, B), NA)
  expect_error(C <- sparse_set_difference(B, A), NA)

  expect_error(C <- sparse_set_union(A, B), NA)
  expect_error(C <- sparse_set_union(B, B), NA)
  expect_error(C <- sparse_set_union(B, A), NA)

})

test_that("fcaR operates on sparse sets II", {

  A <- Matrix(c(FALSE, TRUE, FALSE), nrow = 3, ncol = 1)
  B <- matrix(1, nrow = 3, ncol = 5)

  expect_error(C <- sparse_set_difference(A, B), NA)
  expect_error(C <- sparse_set_difference(B, B), NA)
  expect_error(C <- sparse_set_difference(B, A), NA)

  expect_error(C <- sparse_set_union(A, B), NA)
  expect_error(C <- sparse_set_union(B, B), NA)
  expect_error(C <- sparse_set_union(B, A), NA)

})
