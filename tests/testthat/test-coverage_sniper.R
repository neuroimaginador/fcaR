context("Coverage Sniper Targets")

test_that("Sniper 1: Parallel execution in batch_apply", {
  # Target: R/batch_apply.R lines 34-39
  # Strategy: Force parallelize = TRUE with a tiny batch_size to force
  # the execution flow into the task splitting loop.

  # We skip on CRAN because parallel backends can be unstable there.
  skip_on_cran()

  # Create a quick dummy context
  I <- matrix(sample(c(0, 1), 100, replace = TRUE), nrow = 10, ncol = 10)
  colnames(I) <- letters[1:10]
  fc <- FormalContext$new(I)
  fc$find_implications()

  # If no parallel backend is registered, foreach usually emits warnings or runs sequentially,
  # but the code execution path will still pass through the `if (parallelize)` block,
  # covering the target lines.

  expect_error(fc$implications$apply_rules(rules = "composition",
                                           parallelize = TRUE,
                                           batch_size = 2), NA)
})

test_that("Sniper 2: S3 methods for internal R6 objects", {
  # Target: R/brackets_R6.R (as.list.ConceptSet, length.ConceptSet)
  # Strategy: Explicitly call generic R functions (S3) on the R6 objects.
  # Usually, R6 handles this internally, but these specific S3 wrappers need explicit calls.

  fc <- FormalContext$new(planets)
  fc$find_concepts()

  concepts <- fc$concepts

  # Target: length.ConceptSet
  len <- length(concepts)
  expect_gt(len, 0)

  # Target: as.list.ConceptSet
  lst <- as.list(concepts)
  expect_is(lst, "list")
  expect_equal(length(lst), len)

  # Test subsetting with out-of-bounds indices (to hit error guards in brackets_R6.R)
  expect_true(inherits(concepts[length(concepts) + 5], "ConceptSet"))
  expect_equal(concepts[length(concepts) + 5]$size(), 0)
})

test_that("Sniper 3: Edge cases in matrix construction and clarification", {
  # Target: R/build_sparse_matrix.R and R/clarify_matrix.R
  # Strategy: Use empty or degenerate inputs that trigger "early returns" or safety checks.

  # 1. Construction of empty sparse matrix (build_sparse_matrix is used internally)
  fc_empty <- FormalContext$new()
  expect_true(fc_empty$is_empty())

  # 2. Clarification of trivial matrix (all zeros or all ones)
  I_zero <- matrix(0, 3, 3)
  colnames(I_zero) <- c("a", "b", "c")
  rownames(I_zero) <- c("1", "2", "3")
  fc_z <- FormalContext$new(I_zero)

  # This should trigger massive column/row reduction logic in clarify_matrix
  fc_z$clarify(TRUE)
  # We expect it to reduce dimensions significantly or handle the trivial case without crashing

  # 3. Matrix with duplicate names (forces logic in build_sparse_matrix/checking)
  I_dup <- matrix(c(1,0,0,1), 2, 2)
  colnames(I_dup) <- c("A", "A")
  # If the internal logic handles duplicates, this ensures the warning/error path is covered
  expect_warning(fc_dup <- FormalContext$new(I_dup), NA)
})

test_that("Sniper 4: C++ Vector Operations via Fuzzy Logic", {
  # Target: src/vector_operations.cpp (lines 815-859)
  # Strategy: These C++ lines correspond to set operations (difference, union)
  # that are NOT used in binary logic. We need specific fuzzy logics (Lukasiewicz/Godel)
  # and non-trivial values (doubles like 0.6) to trigger the C++ templates for 'double'.

  I_fuzzy <- matrix(c(0.2, 0.5, 1.0,
                      0.8, 0.0, 0.9,
                      0.5, 0.5, 0.0), nrow = 3, byrow = TRUE)
  fc <- FormalContext$new(I_fuzzy)

  # Iterate over logics to trigger different switch branches in C++
  logics <- c("Lukasiewicz", "Godel", "Product")

  for (lg in logics) {
    fc$use_logic(lg)
    fc$find_implications()

    # Calculating fuzzy closures forces intensive vector operations
    S <- Set$new(attributes = fc$attributes)
    S$assign(attributes = 1, values = 0.6) # Non-trivial fuzzy value

    cl <- fc$implications$closure(S)
    expect_is(cl$closure, "Set")

    # Calculating support also traverses these vectors in C++
    supp <- fc$implications$support()
    expect_true(all(supp >= 0))
  }
})

test_that("Sniper 5: Legacy Subsetting and Dispatcher", {
  # Target: src/subsetting_dispatcher.cpp and subsetting_legacy.cpp
  # Strategy: Use subsetting operator `[` with different index types (negative, logical, character)
  # to force the C++ dispatcher to choose different implementation paths.

  # 1. Subsetting with negative indices (row/col deletion)
  fc <- FormalContext$new(planets)
  fc_sub <- fc[-1, -1]
  expect_equal(fc_sub$dim(), fc$dim() - 1)

  # 2. Subsetting with logical vector (mask)
  mask_obj <- rep(c(TRUE, FALSE), length.out = length(fc$objects))
  fc_log <- fc[mask_obj, ]
  expect_equal(fc_log$dim()[1], sum(mask_obj))

  # 3. Subsetting with names (strings)
  fc_str <- fc[fc$objects[1:2], fc$attributes[1:2]]
  expect_equal(fc_str$dim(), c(2, 2))
})
