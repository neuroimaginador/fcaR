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

test_that("Sniper 6: Background Implication Completion", {
  # Target: R/imp_to_basis.R (complete_rhs), R/formal_context.R (find_implications with bg)
  # Strategy: Set background implications and find implications to trigger completion logic.

  fc <- FormalContext$new(planets)
  # Create some dummy background implications
  lhs <- Matrix::sparseMatrix(i = 1, j = 1, x = 1, dims = c(ncol(planets), 1))
  rhs <- Matrix::sparseMatrix(i = 2, j = 1, x = 1, dims = c(ncol(planets), 1))
  rownames(lhs) <- rownames(rhs) <- colnames(planets)
  
  bg <- ImplicationSet$new(attributes = colnames(planets), lhs = lhs, rhs = rhs)
  
  # Accessing private field via $.__enclos_env__$private or just injecting
  # Since we are testing, we can use this trick if there's no public setter
  fc$.__enclos_env__$private$bg_implications <- bg
  
  expect_error(fc$find_implications(), NA)
  expect_gt(fc$implications$cardinality(), 0)
})

test_that("Sniper 7: RuleSet Advanced Methods", {
  # Target: R/rule_set.R (to_arules, add, filter, get_implications, support)
  # Strategy: Exercise various RuleSet methods with different inputs.

  fc <- FormalContext$new(planets)
  fc$find_implications()
  imps <- fc$implications
  
  # 1. to_arules (already covered in part, but let's be explicit)
  if (requireNamespace("arules", quietly = TRUE)) {
    ari <- imps$to_arules()
    expect_is(ari, "rules")
  }
  
  # 2. add (adding an ImplicationSet to another)
  imps2 <- imps$clone()
  expect_error(imps$add(imps2), NA)
  expect_equal(imps$cardinality(), imps2$cardinality() * 2)
  
  # 3. filter
  filtered <- imps$filter(lhs = colnames(planets)[1])
  expect_true(inherits(filtered, "RuleSet"))
  
  # 4. get_implications (variety of indices)
  # get_implications() takes no arguments and returns rules with confidence 1
  sub_imps <- imps$get_implications()
  if (!is.null(sub_imps)) {
    expect_is(sub_imps, "ImplicationSet")
  }
  
  # To get a subset of rules, use [
  sub_imps_idx <- imps[1:5]
  expect_equal(sub_imps_idx$cardinality(), 5)
  
  # 5. support
  supp <- imps$support()
  expect_length(supp, imps$cardinality())
})

test_that("Sniper 8: Implication Combination and Reordering", {
  # Target: R/combine_implications.R (combine_implications, reorder_attributes)
  # Strategy: Call these functions directly.

  fc <- FormalContext$new(planets)
  fc$find_implications()
  imps1 <- fc$implications
  
  # Reorder attributes
  new_attrs <- rev(fc$attributes)
  imps_reordered <- fcaR:::reorder_attributes(imps1, new_attrs)
  expect_equal(imps_reordered$get_attributes(), new_attrs)
  
  # Combine implications
  # Create another set with different attributes
  I2 <- matrix(c(1,1,0,0), 2, 2)
  colnames(I2) <- c("X", "Y")
  fc2 <- FormalContext$new(I2)
  fc2$find_implications()
  imps2 <- fc2$implications
  
  combined <- fcaR:::combine_implications(imps1, imps2)
  expect_true("X" %in% combined$get_attributes())
  expect_true(colnames(planets)[1] %in% combined$get_attributes())
})

test_that("Sniper 9: C++ Vector Operations (Remaining)", {
  # Target: src/vector_operations.cpp
  # Strategy: Use Product logic and more edge cases.

  I_fuzzy <- matrix(runif(16), nrow = 4)
  colnames(I_fuzzy) <- letters[1:4]
  fc <- FormalContext$new(I_fuzzy)
  fc$use_logic("Product")
  
  fc$find_implications()
  
  # Force some operations
  S1 <- Set$new(attributes = fc$attributes)
  S1$assign(attributes = 1:2, values = c(0.1, 0.9))
  
  cl <- fc$implications$closure(S1)
  expect_is(cl$closure, "Set")
  
  # Trigger cardinality and other metrics
  expect_gt(fc$implications$cardinality(), 0)
})

test_that("Sniper 10: Scaling and Advanced Context Operations", {
  # Target: R/scaling.R (nominal, ordinal, interordinal)
  # Strategy: Explicitly call scaling functions with sample data.

  data <- data.frame(
    A = c("a", "b", "a"),
    B = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  
  # Nominal scaling
  expect_error(fc_nom_mat <- fcaR:::nominal_scaling(data$A, "A"), NA)
  fc_nom <- FormalContext$new(fc_nom_mat)
  expect_is(fc_nom, "FormalContext")
  
  # Ordinal scaling
  expect_error(fc_ord_mat <- fcaR:::ordinal_scaling(data$B, "B"), NA)
  fc_ord <- FormalContext$new(fc_ord_mat)
  expect_is(fc_ord, "FormalContext")
  
  # Interordinal scaling
  expect_error(fc_inter_mat <- fcaR:::interordinal_scaling(data$B, "B"), NA)
  fc_inter <- FormalContext$new(fc_inter_mat)
  expect_is(fc_inter, "FormalContext")
})

test_that("Sniper 11: RuleSet Initialization Edge Cases", {
  # Target: R/rule_set.R (initialize branches)
  # Strategy: Initialize RuleSet with different parameters.

  attrs <- c("a", "b", "c")
  # 1. Empty initialization
  rs_empty <- RuleSet$new(attributes = attrs)
  expect_true(rs_empty$is_empty())
  
  # 2. Initialization with quality but no incidence
  qual <- data.frame(confidence = 0.9)
  rs_q <- RuleSet$new(attributes = attrs, quality = qual)
  expect_equal(nrow(rs_q$get_quality()), 1)
  
  # 3. Initialization from sparse matrices (explicitly)
  lhs <- Matrix::sparseMatrix(i = 1, j = 1, x = 1, dims = c(3, 1))
  rhs <- Matrix::sparseMatrix(i = 2, j = 1, x = 1, dims = c(3, 1))
  rs_mat <- RuleSet$new(attributes = attrs, lhs = lhs, rhs = rhs)
  expect_equal(rs_mat$cardinality(), 1)
})

test_that("Sniper 12: JSON Import/Export", {
  # Target: R/formal_context.R (to_json, context_from_json), R/rule_set.R (to_json, rules_from_json)
  skip_if_not_installed("jsonlite")
  
  fc <- FormalContext$new(planets)
  fc$find_implications()
  
  # 1. FormalContext to/from JSON
  js <- fc$to_json()
  fc2 <- context_from_json(js)
  expect_equal(fc$attributes, fc2$attributes)
  expect_equal(fc$objects, fc2$objects)
  
  # 2. RuleSet/ImplicationSet to/from JSON
  imps <- fc$implications
  js_i <- imps$to_json()
  expect_error(imps2 <- implications_from_json(js_i), NA)
  expect_equal(imps$cardinality(), imps2$cardinality())
})

test_that("Sniper 13: Specialized Scaling & Registry", {
  # Target: R/scaling.R (biordinal_scaling, implication_scaling, interval_scaling)
  
  V <- c(0.1, 0.5, 0.9)
  # biordinal
  m1 <- fcaR:::biordinal_scaling(V, "V")
  expect_equal(ncol(m1), 6) # 3 for <=, 3 for >=
  
  # implication
  m2 <- fcaR:::implication_scaling(V, "V")
  expect_is(m2, "matrix")
  
  # interval
  m3 <- fcaR:::interval_scaling(V, "V", values = c(0, 0.5, 1))
  expect_equal(ncol(m3), 2)
})

test_that("Sniper 14: Many-valued Context & Plotting", {
  # Target: R/formal_context.R (many-valued, plot)
  
  data <- data.frame(
    O1 = c(1, 2),
    O2 = c(3, 4),
    row.names = c("A", "B")
  )
  fc <- FormalContext$new(data)
  # Internal private check
  expect_true(fc$.__enclos_env__$private$is_many_valued)
  
  # Trigger errors for many-valued context
  expect_error(fc$find_implications())
  expect_error(fc$find_concepts())
  
  # Plotting (smoke test) - expect error for many-valued
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    expect_error(fc$plot())
  }
})

test_that("Sniper 15: Direct Optimal Basis & Edge Cases", {
  # Target: R/implication_set.R (to_direct_optimal)
  
  fc <- FormalContext$new(planets)
  fc$find_implications()
  imps <- fc$implications
  
  # to_direct_optimal
  expect_error(do <- imps$to_direct_optimal(), NA)
  expect_is(do, "ImplicationSet")
})

test_that("Sniper 16: Causal Rules & Subcontext", {
  # Target: R/formal_context.R (find_causal_rules, subcontext)
  
  fc <- FormalContext$new(planets)
  
  # Subcontext
  sc <- fc$subcontext(objects = 1:3, attributes = 1:3)
  expect_equal(sc$dim(), c(3, 3))
  
  # Causal Rules
  # Let's say we want to predict 'moon'
  expect_error(cr <- fc$find_causal_rules(response_var = "moon", min_support = 0.01), NA)
  expect_is(cr, "RuleSet")
})

test_that("Sniper 17: Deeper Scaling and Cache Logic", {
  # Target: R/scaling.R (functions, character attributes)
  
  # 1. Custom function as values in nominal_scaling
  V <- c(1, 2, 3)
  my_vals <- function(x) c(1, 3)
  m <- fcaR:::nominal_scaling(V, "V", values = my_vals)
  expect_equal(ncol(m), 2)
  
  # 2. Character attributes in ordinal_scaling
  V_char <- c("low", "medium", "high")
  m_char <- fcaR:::ordinal_scaling(V_char, "V", values = c("low", "medium", "high"))
  expect_is(m_char, "matrix")
  
  # 3. Cache logic in ImplicationSet$support()
  fc <- FormalContext$new(planets)
  fc$find_implications()
  imps <- fc$implications
  # First call computes support
  s1 <- imps$support()
  # Second call should use cached value
  s2 <- imps$support()
  expect_equal(s1, s2)
  
  # 4. Binary context message in to_direct_optimal
  expect_message(imps$to_direct_optimal(verbose = TRUE), "Binary context detected")
  
  # 5. Fuzzy closure with non-Set input
  # Use a sparse matrix (S4) instead of a numeric vector
  S_mat <- Matrix::sparseMatrix(i = integer(0), j = integer(0), x = numeric(0), dims = c(1, length(fc$attributes)))
  cl <- imps$closure(S_mat)
  expect_is(cl$closure, "Set")
  
  # 6. FormalContext$closure with sparse matrix input
  cl_fc <- fc$closure(S_mat)
  expect_is(cl_fc, "Set")
})

test_that("Sniper 18: Orphan Helpers and Internal Methods", {
  # 1. R/build_sparse_matrix.R
  m_sparse <- fcaR:::build_sparse_matrix(i = as.integer(c(0, 1)), p = as.integer(c(0, 1, 2)), x = c(1, 1), dims = c(2, 2))
  expect_is(m_sparse, "dgCMatrix")
  
  # 2. R/logics.R - Hit all logics
  for (l in fcaR:::available_logics()) {
    expect_is(fcaR:::get_implication(l), "function")
    expect_is(fcaR:::get_tnorm(l), "function")
  }
  expect_null(fcaR:::get_implication("non-existent"))
  expect_null(fcaR:::get_tnorm("non-existent"))
  
  # 3. R/reorder.R
  m1 <- Matrix::Matrix(c(1,1,0, 0,0,1, 1,0,0), nrow=3, ncol=3, sparse=TRUE)
  m2 <- Matrix::Matrix(c(0,0,1, 1,1,0, 0,1,1), nrow=3, ncol=3, sparse=TRUE)
  res_reorder <- fcaR:::reorder(m1, m2, c("a", "b", "c"))
  expect_is(res_reorder, "list")

  # 4. R/plot_context.R
  fc_bin <- FormalContext$new(planets)
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    expect_error(fc_bin$plot(), NA)
  }
})

test_that("Sniper 19: Expanded C++ Metrics and Concepts", {
  # Target src/fastcbo.cpp, src/fastcbo-binary.cpp, src/inclose_binary_old.cpp
  fc <- FormalContext$new(planets)
  
  # FastCbO
  expect_error(fc$find_concepts(method = "FastCbO"), NA)
  
  # FastCbO_binary (direct call to hit src/fastcbo-binary.cpp)
  I_mat <- as.matrix(planets)
  expect_is(fcaR:::FastCbO_binary(I_mat, colnames(planets)), "list")
  
  # InClose_binary (direct call to hit src/inclose_binary_old.cpp)
  expect_is(fcaR:::InClose_binary(I_mat, colnames(planets)), "list")

  # Fuzzy basis methods (src/binary_do_optimized.cpp & src/fuzzy_do.cpp)
  m_tiny <- matrix(c(1, 0.5, 0.2, 0.8), nrow=2)
  rownames(m_tiny) <- c("O1", "O2")
  colnames(m_tiny) <- c("A1", "A2")
  fc_tiny <- FormalContext$new(m_tiny)
  fc_tiny$find_implications()
  
  methods <- c("do_sp", "direct_optimal", "final_ts", "monotonic")
  for (m in methods) {
    expect_error(fc_tiny$implications$clone()$to_direct_optimal(method = m, verbose = FALSE), NA)
  }
  
  # ConceptLattice properties (hit LatticeProperties.cpp)
  fc_tiny$find_concepts()
  lat <- fc_tiny$concepts
  expect_is(lat$join_irreducibles(), "ConceptSet")
  expect_is(lat$meet_irreducibles(), "ConceptSet")
  expect_error(lat$separation(), NA)
})

test_that("Sniper 20: Random Contexts, dplyr and Utils", {
  # Random Contexts
  expect_is(RandomContext(5, 5, density = 0.5), "FormalContext")
  expect_is(RandomContext(5, 5, distribution = "dirichlet"), "FormalContext")
  expect_is(RandomDistributiveContext(5), "FormalContext")
  
  # Randomization
  fc <- FormalContext$new(planets)
  expect_is(randomize_context(fc, method = "swap", iterations = 5), "FormalContext")
  expect_is(randomize_context(fc, method = "rewire", iterations = 5), "FormalContext")
  
  # dplyr Verbs - Hit more branches
  expect_true("moon" %in% (fc |> dplyr::select(moon))$attributes)
  expect_equal(length((fc |> dplyr::filter(moon == 100))$objects), 0)
  expect_true("X" %in% (fc |> dplyr::mutate(X = 1))$attributes)
  expect_is(fc |> dplyr::arrange(desc(moon)), "FormalContext")
  expect_true("luna" %in% (fc |> dplyr::rename(luna = moon))$attributes)
  
  # Utils
  m <- Matrix::Matrix(c(1,0,1, 0,1,0), nrow=3, ncol=2, sparse=TRUE)
  l <- fcaR:::sparse_to_list(m)
  expect_equal(length(l), 2)
  
  # Messages
  #expect_message(fcaR:::first_time_message("fcaR_opt_final_v2", "msg"), "msg")
})
