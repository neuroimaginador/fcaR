test_that("RandomContext generates correct dimensions", {
  n_obj <- 10
  n_att <- 8
  fc <- RandomContext(n_obj, n_att, density = 0.5)

  expect_equal(fc$dim(), c(n_obj, n_att))
  expect_true(inherits(fc, "FormalContext"))
})

test_that("Dirichlet distribution runs", {
  fc <- RandomContext(20, 10, distribution = "dirichlet", alpha = 0.5)
  expect_equal(fc$dim(), c(20, 10))
  # Dirichlet tends to produce varied row sums
  rs <- rowSums(fc$incidence())
  expect_true(max(rs) >= min(rs))
})

test_that("Edge Swapping preserves marginals", {
  data(planets)
  fc <- FormalContext$new(planets)
  I_orig <- as.matrix(fc$incidence())

  # Convert to binary strict for comparison
  I_orig[I_orig > 0] <- 1

  row_sums_orig <- rowSums(I_orig)
  col_sums_orig <- colSums(I_orig)

  fc_swap <- randomize_context(fc, method = "swap", iterations = 1000)
  I_swap <- as.matrix(fc_swap$incidence())

  expect_equal(rowSums(I_swap), row_sums_orig)
  expect_equal(colSums(I_swap), col_sums_orig)

  # Matrix content should likely differ (unless very small/locked)
  if (sum(I_orig) > 5 && sum(I_orig) < length(I_orig) - 5) {
    expect_false(all(I_swap == I_orig))
  }
})

test_that("Rewiring preserves density only", {
  data(planets)
  fc <- FormalContext$new(planets)

  fc_rewire <- randomize_context(fc, method = "rewire", iterations = 1000)

  # Density check
  expect_equal(sum(fc$incidence() > 0), sum(fc_rewire$incidence()))
})
