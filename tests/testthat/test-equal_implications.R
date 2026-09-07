context("equal_implications with Trie")

test_that("equal_implications checks equality regardless of order (binary)", {
  data("planets")
  fc <- FormalContext$new(planets)
  fc$find_implications()
  
  imps1 <- fc$implications$clone()
  n <- imps1$cardinality()
  expect_gt(n, 0)
  
  # 1. Identical sets
  expect_true(equal_implications(imps1, imps1))
  expect_true(imps1 %==% imps1)
  expect_true(imps1$equals(imps1))
  
  # 2. Permuted rules
  set.seed(42)
  shuffled_idx <- sample(n)
  imps2 <- imps1[shuffled_idx]
  
  expect_true(equal_implications(imps1, imps2))
  expect_true(equal_implications(imps2, imps1))
  expect_true(imps1 %==% imps2)
  expect_true(imps2 %==% imps1)
  expect_true(imps1$equals(imps2))
  
  # 3. Different cardinalities
  imps_subset <- imps1[1:(n - 1)]
  expect_false(equal_implications(imps1, imps_subset))
  expect_false(equal_implications(imps_subset, imps1))
  expect_false(imps1 %==% imps_subset)
  
  # 4. Same cardinality but different rules
  imps_mod <- imps1$clone()
  # Swap a rule with a duplicate of another
  imps_mod2 <- imps1[c(1, 1, 3:n)]
  expect_false(equal_implications(imps1, imps_mod2))
})

test_that("equal_implications handles permuted and incompatible attributes", {
  data("planets")
  fc1 <- FormalContext$new(planets)
  fc1$find_implications()
  imps1 <- fc1$implications$clone()
  
  # Context with permuted attribute columns
  set.seed(123)
  perm_cols <- sample(ncol(planets))
  planets_perm <- planets[, perm_cols]
  fc2 <- FormalContext$new(planets_perm)
  fc2$find_implications()
  imps2 <- fc2$implications$clone()
  
  # Even with different attribute column orders, sets of implications should be equal
  expect_true(equal_implications(imps1, imps2))
  expect_true(imps1 %==% imps2)
  
  # Different attributes altogether
  fc3_data <- planets
  colnames(fc3_data)[1] <- "UnknownAttribute"
  fc3 <- FormalContext$new(fc3_data)
  fc3$find_implications()
  imps3 <- fc3$implications$clone()
  
  expect_false(equal_implications(imps1, imps3))
})

test_that("equal_implications handles empty sets and empty LHS", {
  data("planets")
  fc <- FormalContext$new(planets)
  
  # Empty implication sets
  empty1 <- ImplicationSet$new(attributes = colnames(planets))
  empty2 <- ImplicationSet$new(attributes = colnames(planets))
  
  expect_true(equal_implications(empty1, empty2))
  expect_true(empty1 %==% empty2)
  
  fc$find_implications()
  expect_false(equal_implications(fc$implications, empty1))
  
  # Implication with empty LHS
  # If an attribute is present in all objects, it appears with empty LHS
  planets_all <- cbind(planets, common_attr = 1)
  fc_all <- FormalContext$new(planets_all)
  fc_all$find_implications()
  imps_all <- fc_all$implications$clone()
  
  shuffled_all <- imps_all[sample(imps_all$cardinality())]
  expect_true(equal_implications(imps_all, shuffled_all))
  expect_true(imps_all %==% shuffled_all)
})

test_that("equal_implications works with fuzzy implications", {
  objects <- paste0("O", 1:6)
  attributes <- paste0("P", 1:6)
  
  I <- matrix(data = c(0, 1, 0.5, 0, 0, 0.5,
                       1, 1, 0.5, 0, 0, 0,
                       0.5, 1, 0, 0, 1, 0,
                       0.5, 0, 0, 1, 0.5, 0,
                       1, 0, 0, 0.5, 0, 0,
                       0, 0, 1, 0, 0, 0),
              nrow = length(objects),
              byrow = FALSE)
  colnames(I) <- attributes
  rownames(I) <- objects
  
  fc <- FormalContext$new(I = I)
  fc$find_implications()
  
  fuzzy_imps1 <- fc$implications$clone()
  n <- fuzzy_imps1$cardinality()
  expect_gt(n, 0)
  
  # Permuted fuzzy implications
  shuffled_fuzzy <- fuzzy_imps1[sample(n)]
  expect_true(equal_implications(fuzzy_imps1, shuffled_fuzzy))
  expect_true(fuzzy_imps1 %==% shuffled_fuzzy)
  
  # Non-RuleSet argument throws error
  expect_error(equal_implications(fuzzy_imps1, list()))
})
