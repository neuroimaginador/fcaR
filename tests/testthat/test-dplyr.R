context("dplyr Integration for FormalContext and ImplicationSet")

library(dplyr)
# Use a known dataset
data("planets")

test_that("FormalContext: select() works with names and tidyselect helpers", {
  fc <- FormalContext$new(planets)

  # Select by name
  fc_sub <- fc |> select(moon, large)
  expect_equal(fc_sub$dim()[2], 2)
  expect_true(all(c("moon", "large") %in% fc_sub$attributes))

  # Select by helper
  fc_sub2 <- fc |> select(starts_with("no_"))
  expect_true("no_moon" %in% fc_sub2$attributes)
})

test_that("FormalContext: filter() subsets objects correctly", {
  fc <- FormalContext$new(planets)
  n_orig <- fc$dim()[1]

  # Standard filter
  fc_large <- fc |> filter(large == 1)
  expect_lt(fc_large$dim()[1], n_orig)

  # Empty filter (User's fix verification)
  fc_empty <- fc |> filter(large == 1, small == 1)
  expect_equal(fc_empty$dim()[1], 0)
  expect_equal(length(fc_empty$attributes), length(fc$attributes))
})

test_that("FormalContext: mutate() adds new attributes", {
  fc <- FormalContext$new(planets)

  fc_new <- fc |> mutate(is_giant = large == 1 & no_moon == 0)
  expect_true("is_giant" %in% fc_new$attributes)

  # Check value in data
  inc <- as.matrix(fc_new$incidence())
  if ("Jupiter" %in% rownames(inc)) {
    expect_equal(inc["Jupiter", "is_giant"], 1)
  }
})

test_that("FormalContext: rename() changes attribute names", {
  fc <- FormalContext$new(planets)

  fc_ren <- fc |> rename(satellite = moon)
  expect_true("satellite" %in% fc_ren$attributes)
  expect_false("moon" %in% fc_ren$attributes)
})

test_that("ImplicationSet: filter() works with metrics and semantics", {
  fc <- FormalContext$new(planets)
  fc$find_implications()
  imps <- fc$implications

  # Metrics filter (safe threshold)
  strong_imps <- imps |> filter(support > 0.25)
  if (strong_imps$cardinality() > 0) {
    expect_true(all(strong_imps$support() > 0.25))
  }

  # Semantic filter
  lhs_large <- imps |> filter(lhs_has("large"))
  if (lhs_large$cardinality() > 0) {
    lhs_mat <- lhs_large$get_LHS_matrix()
    large_idx <- match("large", imps$get_attributes())
    expect_true(all(lhs_mat[large_idx, ] == 1))
  }
})

test_that("ImplicationSet: arrange() and slice() work together", {
  fc <- FormalContext$new(planets)
  fc$find_implications()
  imps <- fc$implications

  sorted_imps <- imps |>
    arrange(desc(support))
  supps <- sorted_imps$support()
  expect_true(!is.unsorted(rev(supps)))

  top_3 <- sorted_imps |> slice(1:3)
  expect_equal(top_3$cardinality(), 3)

  # Empty slice
  empty_slice <- sorted_imps |> slice(0)
  expect_equal(empty_slice$cardinality(), 0)
})
