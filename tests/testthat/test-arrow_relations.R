library(testthat)
library(fcaR)

context("Arrow relations")

test_that("Arrow relations are computed correctly", {
  # Example from Ganter & Wille, Fig 2.3
  I <- matrix(c(
    1, 1, 0, 0, 0,
    1, 0, 1, 0, 0,
    0, 0, 0, 1, 1,
    0, 0, 0, 0, 1
  ), nrow = 4, byrow = TRUE)
  colnames(I) <- letters[1:5]
  rownames(I) <- 1:4

  fc <- FormalContext$new(I)
  fc$calculate_arrow_relations()
  arrows <- fc$get_arrow_relations()

  # (1, c) should be double arrow (3)
  expect_equal(arrows["1", "c"], 3)

  # 4-a should be nearrow (2) because {1, 2} \subsetneq {1, 2, 3, 4} and 4 \in {1, 2, 3, 4}
  expect_equal(arrows["4", "a"], 2)

  # Check print and latex don't error
  expect_output(print(fc))
  expect_output(fc$to_latex())
})

test_that("Arrow relations error on non-binary context", {
  I <- matrix(c(1, 0.5, 0, 1), nrow = 2)
  fc <- FormalContext$new(I)
  expect_error(fc$calculate_arrow_relations())
})
