test_that("The parser parses", {

  input <- system.file("implications", "ex_implications",
                       package = "fcaR")
  expect_error(imps <- parse_implications(input), NA)

  expect_true(inherits(imps, "ImplicationSet"))
  expect_equal(imps$cardinality(), 3)
  expect_equal(imps$get_attributes(), letters[1:4])

})
