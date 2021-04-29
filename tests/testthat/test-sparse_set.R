context("Set")

test_that("fcaR operates on sparse sets", {

  A <- matrix(1, nrow = 10, ncol = 1)
  B <- matrix(1, nrow = 10, ncol = 5)

  expect_error(print(A), NA)

  expect_error(.difference2(A, B), NA)
  expect_error(.difference2(B, B), NA)
  expect_error(.difference2(B, A), NA)

  expect_error(.union(A, B), NA)
  expect_error(.union(B, B), NA)
  expect_error(.union(B, A), NA)

})

test_that("fcaR operates on sparse sets II", {

  A <- Matrix::Matrix(c(FALSE, TRUE, FALSE), nrow = 3, ncol = 1)
  B <- matrix(1, nrow = 3, ncol = 5)

  expect_error(.difference2(A, B), NA)
  expect_error(.difference2(B, B), NA)
  expect_error(.difference2(B, A), NA)

  expect_error(.union(A, B), NA)
  expect_error(.union(B, B), NA)
  expect_error(.union(B, A), NA)

})

test_that("fcaR uses class Set", {

  attributes <- paste0("P", 1:6)

  expect_error(A <- Set$new(attributes = attributes), NA)
  expect_error(A$assign(attributes = "P1", values = 0.3), NA)

  expect_error(A["P1"], NA)
  expect_is(A["P1"], "Set")
  expect_equal(A["P1"]$cardinal(), 0.3)

  expect_is(A$get_vector(), "Matrix")

  expect_equal(A$get_attributes(), attributes)

  expect_equal(A$length(), 6)

  expect_output(A$print())

  expect_is(A$to_latex(FALSE), "character")
  expect_is(capture_output(A$to_latex(TRUE)), "character")

  expect_true(A %<=% A)

  expect_error(v <- as_vector(A), NA)
  expect_error(A2 <- as_Set(v), NA)

  expect_true(A %==% A2)

})

test_that("fcaR computes differences of Sets", {

  attributes <- paste0("P", 1:6)

  expect_error(A <- Set$new(attributes = attributes), NA)
  expect_error(A$assign(attributes = "P1", values = 0.5), NA)

  expect_error(B <- Set$new(attributes = attributes), NA)
  expect_error(B$assign(attributes = "P1", values = 0.3), NA)

  expect_error(A %-% B, NA)
  expect_error(B %-% A, NA)

})
