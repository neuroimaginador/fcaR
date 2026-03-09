test_that("Bonds computation methods (conexp, mcis) are consistent", {
  # Create two small formal contexts
  set.seed(42)
  mat1 <- matrix(sample(0:1, 9, replace = TRUE), nrow = 3, ncol = 3)
  rownames(mat1) <- paste0("O", 1:3)
  colnames(mat1) <- paste0("A", 1:3)
  fc1 <- FormalContext$new(mat1)

  mat2 <- matrix(sample(0:1, 9, replace = TRUE), nrow = 3, ncol = 3)
  rownames(mat2) <- paste0("P", 1:3)
  colnames(mat2) <- paste0("B", 1:3)
  fc2 <- FormalContext$new(mat2)

  # Compute bonds with both methods
  bl_conexp <- bonds(fc1, fc2, method = "conexp")
  bl_mcis <- bonds(fc1, fc2, method = "mcis")

  # Sizes should match
  expect_equal(bl_conexp$size(), bl_mcis$size())
  expect_gt(bl_conexp$size(), 0)

  # Intents (the bonds themselves) should be identical
  intents_conexp <- as.matrix(bl_conexp$intents())
  intents_mcis <- as.matrix(bl_mcis$intents())
  
  # Remove dimnames for comparison as they might differ (C1, C2...)
  dimnames(intents_conexp) <- NULL
  dimnames(intents_mcis) <- NULL

  # Normalize order of bonds (columns)
  norm_conexp <- intents_conexp[, order(apply(intents_conexp, 2, paste, collapse="")), drop=FALSE]
  norm_mcis <- intents_mcis[, order(apply(intents_mcis, 2, paste, collapse="")), drop=FALSE]

  expect_equal(norm_conexp, norm_mcis)
})

test_that("is_bond correctly verifies relations", {
  set.seed(42)
  fc1 <- FormalContext$new(matrix(sample(0:1, 4, replace = TRUE), 2, 2))
  fc2 <- FormalContext$new(matrix(sample(0:1, 4, replace = TRUE), 2, 2))

  bl <- bonds(fc1, fc2)
  all_bonds <- bl$get_bonds()

  # A valid bond from the lattice should pass
  expect_true(is_bond(fc1, fc2, all_bonds[[1]]))

  # The core bond should pass
  core <- bl$get_core()
  expect_true(is_bond(fc1, fc2, core))

  # An empty relation is NOT always a bond (only if empty set is an intent of K2 and extent of K1)
  # But is_bond should at least return a logical
  res <- is_bond(fc1, fc2, matrix(0, 2, 2))
  expect_type(res, "logical")
})

test_that("BondLattice methods and similarities work", {
  set.seed(42)
  # Use very small context to speed up width/dimension
  fc1 <- FormalContext$new(matrix(sample(0:1, 4, replace = TRUE), 2, 2))
  fc2 <- fc1$dual()

  bl <- bonds(fc1, fc2)

  # Check BondLattice inheritance and basic fields
  expect_s3_class(bl, "BondLattice")
  
  # get_core returns a FormalContext
  core <- bl$get_core()
  expect_s3_class(core, "FormalContext")

  # Similarity metrics
  metrics <- c("log-bond", "top-density", "complexity", "core-agreement", "entropy", "stability")
  for (m in metrics) {
    val <- bl$similarity(m)
    expect_type(val, "double")
  }

  # Order-theoretic metrics
  expect_type(bl$similarity("width"), "integer")
  expect_type(bl$similarity("dimension"), "integer")
})

test_that("Edge cases for bonds", {
  # Single object/attribute context [1]
  m2 <- matrix(1, 1, 1)
  fc3 <- FormalContext$new(m2)
  fc4 <- FormalContext$new(m2)
  bl2 <- bonds(fc3, fc4)
  # In [1], the only intent is {a} and only extent is {g}.
  # So the only bond is [1].
  expect_equal(bl2$size(), 1)
})
