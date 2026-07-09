context("Matrix Factorization")

test_that("Factorization error for fuzzy contexts", {
  # Create a fuzzy context
  I_fuzzy <- matrix(
    c(0.9, 0.1, 0.8, 0.2, 1.0, 0.0, 0.8, 0.0, 0.9),
    nrow = 3,
    byrow = TRUE
  )
  colnames(I_fuzzy) <- c("a", "b", "c")
  rownames(I_fuzzy) <- c("o1", "o2", "o3")

  fc_fuzzy <- FormalContext$new(I_fuzzy)
  expect_error(
    fc_fuzzy$factorize(method = "GreConD"),
    "Current context is fuzzy"
  )
})

test_that("Factorization works for boolean contexts and recovers dimensions", {
  # Create a boolean context
  I_bin <- matrix(c(
    1, 1, 0, 0, 1,
    1, 1, 0, 0, 0,
    0, 0, 1, 1, 1,
    0, 0, 1, 1, 0,
    1, 0, 0, 0, 1
  ), nrow = 5, byrow = TRUE)
  rownames(I_bin) <- paste0("O", 1:5)
  colnames(I_bin) <- paste0("A", 1:5)

  fc <- FormalContext$new(I_bin)

  # Test GreConD
  res_grecond <- fc$factorize(method = "GreConD")
  expect_false(is.null(res_grecond))
  expect_is(res_grecond$object_factor, "FormalContext")
  expect_is(res_grecond$factor_attribute, "FormalContext")
  expect_equal(res_grecond$object_factor$dim()[1], 5) # objects
  expect_equal(res_grecond$factor_attribute$dim()[2], 5) # attributes
  expect_equal(
    res_grecond$object_factor$dim()[2],
    res_grecond$factor_attribute$dim()[1]
  )

  # Test ASSO
  res_asso <- fc$factorize(method = "ASSO", threshold = 0.6)
  expect_false(is.null(res_asso))
  expect_is(res_asso$object_factor, "FormalContext")

  # Test other BMF methods
  methods <- c("RSF", "RSF-ES", "GreEss", "PaNDa+ MDL", "Hyper")
  for (m in methods) {
    res <- fc$factorize(method = m)
    if (!is.null(res)) {
      expect_is(res$object_factor, "FormalContext")
      expect_is(res$factor_attribute, "FormalContext")
    }
  }

  # Unknown method error
  expect_error(
    fc$factorize(method = "UnknownMethod"),
    "Unknown factorization method"
  )

  # Empty context error
  fc_empty <- FormalContext$new(matrix(0, nrow = 2, ncol = 2))
  expect_warning(fc_empty$factorize(method = "GreConD"), "No factors found.")
})

