test_that("Metrics calculation works", {
  I <- matrix(c(
    1, 0, 1,
    1, 1, 0,
    0, 1, 0
  ), nrow = 3, byrow = TRUE)
  fc <- FormalContext$new(I)
  fc$find_concepts()

  # Stability
  stab <- fc$concepts$stability()
  expect_equal(length(stab), fc$concepts$size())
  expect_true(all(stab >= 0 & stab <= 1))

  # Separation
  sep <- fc$concepts$separation()
  expect_equal(length(sep), fc$concepts$size())
  # Separation of bottom concept (extent size 0 or 1 depending on logic) should be valid

  # Density
  dens <- fc$concepts$density(fc$incidence())
  expect_equal(length(dens), fc$concepts$size())
  expect_true(all(dens == 1 | dens == 0)) # Binary context -> density 1
})
