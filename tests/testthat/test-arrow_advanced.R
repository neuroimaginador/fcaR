library(testthat)
library(fcaR)

context("Advanced Arrow Relations")

test_that("is_distributive works", {
  # N5 (Pentagon) - Not distributive
  I_n5 <- matrix(c(
    1, 0, 0,
    1, 1, 0,
    0, 0, 1
  ), nrow = 3, byrow = TRUE)
  fc_n5 <- FormalContext$new(I_n5)
  expect_false(fc_n5$is_distributive())

  # Context for a distributive lattice (e.g. 2x2 identity)
  I_dist <- matrix(c(
    1, 0,
    0, 1
  ), nrow = 2)
  fc_dist <- FormalContext$new(I_dist)
  expect_true(fc_dist$is_distributive())
})

test_that("get_irreducible works and handles redundancy", {
  # Planets dataset - Mercury and Venus are redundant
  fc <- FormalContext$new(planets)
  
  # Before clarification
  fc$calculate_arrow_relations()
  irr_objs <- fc$get_irreducible_objects()
  # Mercury and Venus both have arrows because they are maximal in this matrix
  expect_true("Mercury" %in% irr_objs)
  expect_true("Venus" %in% irr_objs)

  # Hand-made redundant example
  I <- matrix(c(
    1, 0,
    1, 0,
    0, 1
  ), nrow = 3, byrow = TRUE)
  rownames(I) <- c("O1", "O2", "O3")
  fc2 <- FormalContext$new(I)
  fc2$calculate_arrow_relations()
  # Both O1 and O2 should have swarrow because they are maximal (no proper supersets)
  expect_true(all(c("O1", "O2") %in% fc2$get_irreducible_objects()))
})

test_that("reduce_arrows works", {
  # planets has 9 objects
  fc <- FormalContext$new(planets)
  fc_red <- fc$reduce_arrows()
  
  # After clarification (Mercury/Venus merged, etc.) and reduction
  # Original planets: 9 objects (Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto)
  # Mercury = Venus
  # Earth = Mars
  # Jupiter = Saturn
  # Uranus = Neptune
  # So clarified has 5 objects: Mercury, Earth, Jupiter, Uranus, Pluto.
  # Are any of these reducible?
  # Small-Near-NoMoon: Mercury
  # Small-Near-Moon: Earth
  # Large-Far-Moon: Jupiter
  # Medium-Far-Moon: Uranus
  # Small-Far-Moon: Pluto
  # All seem irreducible in this context.
  expect_equal(length(fc_red$objects), 5)
})

test_that("Consistency of reduction methods", {
  I <- matrix(c(1, 1, 0,
                1, 1, 0,
                0, 0, 1), nrow = 3, byrow = TRUE)
  fc <- FormalContext$new(I)
  
  fc_std <- fc$standardize()
  fc_core <- fc$get_core()
  fc_red <- fc$reduce(copy = TRUE)
  
  expect_equal(fc_std$incidence(), fc_core$incidence())
  expect_equal(fc_std$incidence(), fc_red$incidence())
  
  # Check in-place mutation
  fc$reduce(copy = FALSE)
  expect_equal(fc$incidence(), fc_std$incidence())
})

test_that("get_core works", {
  # Simple context where some arrows are double
  I <- matrix(c(
    1, 1, 0,
    1, 0, 1,
    0, 0, 0
  ), nrow = 3, byrow = TRUE)
  fc <- FormalContext$new(I)
  fc$calculate_arrow_relations()
  # (3, 1) has 3'={}. Intents containing {}: {a,b}, {a,c}, {full}.
  # Proper supersets: {a,b}, {a,c}, {full}. Intersection: {a}.
  # Col 1 extends: {1, 2}. Proper supersets among extents: {1, 2, 3}.
  # Row 3 is in {1,2,3}? Yes.
  # So (3, 1) is double arrow.
  
  core <- fc$get_core()
  expect_true(nrow(core$incidence()) > 0)
})
