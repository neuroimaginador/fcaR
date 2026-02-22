library(testthat)
library(fcaR)

context("Direct Optimal Basis - Binary Optimizations")

test_that("to_direct_optimal() correctly handles binary and fuzzy contexts", {
  # 1. Binary Context Case
  fc_bin <- FormalContext$new(planets)
  fc_bin$find_implications()
  base_card <- fc_bin$implications$cardinality()
  
  # Trigger binary fast-path (implicitly via to_direct_optimal default do_sp)
  expect_message(
    res_bin <- fc_bin$implications$to_direct_optimal(verbose = TRUE),
    "Binary context detected"
  )
  
  expect_is(res_bin, "ImplicationSet")
  # Semantic equivalence with original basis
  expect_true(res_bin %~% fc_bin$implications)
  
  # 2. Fuzzy Context Case
  I_fuzzy <- matrix(c(1, 0.5, 0, 0,
                      0.8, 1, 0.2, 0,
                      0, 0.2, 1, 0.5,
                      0, 0, 0.6, 1), nrow = 4, byrow = TRUE)
  colnames(I_fuzzy) <- letters[1:4]
  fc_fuzzy <- FormalContext$new(I_fuzzy)
  fc_fuzzy$find_implications()
  
  # Should NOT trigger binary fast-path
  # We check the message is NOT present
  # Note: to_direct_optimal returns the object invisibly
  expect_no_message(
    fc_fuzzy$implications$to_direct_optimal(verbose = TRUE)
  )
  
  expect_is(fc_fuzzy$implications, "ImplicationSet")
})

test_that("All binary Direct Optimal implementations are semantically equivalent", {
  # Small context to avoid long test times but large enough to be interesting
  I <- matrix(c(1, 0, 1, 1, 0,
                0, 1, 0, 1, 1,
                1, 1, 0, 0, 1,
                0, 0, 1, 0, 1,
                1, 0, 0, 1, 1), nrow = 5, byrow = TRUE)
  colnames(I) <- letters[1:5]
  fc <- FormalContext$new(I)
  fc$find_implications()
  
  original_imps <- fc$implications$clone()
  
  # We test our new optimized wrappers directly if exported, 
  # but here we test them via to_direct_optimal if they were added to the method list,
  # or we rely on the internal routing.
  
  # Note: Since I integrated the 'Tree Closure' as the default for binary 'do_sp',
  # it's covered by the previous test.
  
  # Let's verify that even if we manually select different general methods, 
  # the binary fast-path (Tree) still takes over if it's a binary context.
  methods <- c("do_sp", "direct_optimal", "monotonic", "priority")
  
  for (m in methods) {
    imps_test <- original_imps$clone()
    # All these should trigger the binary tree-closure fast-path message
    expect_message(
      imps_test$to_direct_optimal(method = m, verbose = TRUE),
      "Binary context detected"
    )
    expect_true(imps_test %~% original_imps)
  }
})

test_that("Fuzzy methods still yield correct results", {
  # Ensure my changes didn't break the fallback for fuzzy contexts
  I_fuzzy <- matrix(c(1, 0.2, 0,
                      0.5, 1, 0.3,
                      0, 0.1, 1), nrow = 3, byrow = TRUE)
  colnames(I_fuzzy) <- letters[1:3]
  fc <- FormalContext$new(I_fuzzy)
  fc$find_implications()
  
  original <- fc$implications$clone()
  
  # Try fuzzy priority refinement
  fc$implications$to_direct_optimal(method = "priority")
  
  expect_true(fc$implications %~% original)
})
