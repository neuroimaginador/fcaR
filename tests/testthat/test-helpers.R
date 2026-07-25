test_that("attribute_set and object_set construct valid Set objects", {
  fc <- FormalContext$new(planets)

  # attributes: "large" (3) and "near" (4)
  S_att <- attribute_set(fc, "near", "large")
  expect_true(inherits(S_att, "Set"))
  expect_equal(S_att$get_attributes(), fc$attributes)
  expect_equal(as.numeric(S_att$get_vector()), c(0, 0, 1, 1, 0, 0, 0))

  # objects: "Mercury" (1) and "Venus" (2)
  S_obj <- object_set(fc, "Mercury", "Venus")
  expect_true(inherits(S_obj, "Set"))
  expect_equal(S_obj$get_attributes(), fc$objects)
  expect_equal(as.numeric(S_obj$get_vector()), c(1, 1, 0, 0, 0, 0, 0, 0, 0))
})

test_that("FormalContext intent, extent, closure, and is_closed handle ... ellipsis correctly", {
  fc <- FormalContext$new(planets)

  # intent (objects)
  int_set_manual <- fc$intent(object_set(fc, "Mercury", "Venus"))
  int_set_dots <- fc$intent("Mercury", "Venus")
  expect_true(int_set_manual %==% int_set_dots)

  # extent (attributes)
  ext_set_manual <- fc$extent(attribute_set(fc, "near", "medium"))
  ext_set_dots <- fc$extent("near", "medium")
  expect_true(ext_set_manual %==% ext_set_dots)

  # closure (attributes)
  clos_set_manual <- fc$closure(attribute_set(fc, "near"))
  clos_set_dots <- fc$closure("near")
  expect_true(clos_set_manual %==% clos_set_dots)

  # is_closed
  expect_false(fc$is_closed("near"))
  expect_true(fc$is_closed(clos_set_dots))
})

test_that("ConceptSet intents(as_list=TRUE) and extents(as_list=TRUE) return lists of Sets", {
  fc <- FormalContext$new(planets)
  fc$find_concepts()

  ints_list <- fc$concepts$intents(as_list = TRUE)
  exts_list <- fc$concepts$extents(as_list = TRUE)

  expect_equal(length(ints_list), fc$concepts$size())
  expect_equal(length(exts_list), fc$concepts$size())

  expect_true(inherits(ints_list[[1]], "Set"))
  expect_true(inherits(exts_list[[1]], "Set"))

  # Check equality of matrix columns and corresponding Set vectors
  ints_mat <- fc$concepts$intents()
  expect_equal(as.numeric(ints_list[[1]]$get_vector()), as.numeric(ints_mat[, 1]))
})

test_that("sublattice_from on ConceptLattice behaves correctly", {
  fc <- FormalContext$new(planets)
  fc$find_concepts()

  # Filter by attribute "near" and top_n
  sub <- fc$concepts$sublattice_from(attributes = "near", top_n = 3, verbose = FALSE)
  expect_true(inherits(sub, "ConceptLattice"))
  expect_true(sub$size() <= fc$concepts$size())

  # Filter by match=any
  sub_any <- fc$concepts$sublattice_from(attributes = c("near", "far"), match = "any", top_n = NULL, verbose = FALSE)
  expect_true(inherits(sub_any, "ConceptLattice"))
})

test_that("RuleSet total_size matches colSums of size()", {
  fc <- FormalContext$new(planets)
  fc$find_implications()

  ts <- fc$implications$total_size()
  expect_equal(length(ts), 2)
  expect_equal(names(ts), c("LHS", "RHS"))
  expect_equal(ts, colSums(fc$implications$size()))
})

test_that("recommendation_table and iterative_recommender execute successfully", {
  fc <- FormalContext$new(planets)
  fc$find_implications()

  S <- attribute_set(fc, "near")
  recoms <- fc$implications$recommend(S, attribute_filter = fc$attributes)

  tab <- recommendation_table(recoms)
  expect_true(is.data.frame(tab))
  expect_equal(colnames(tab), c("attribute", "score"))

  # Query parameter check
  tab_q <- recommendation_table(recoms, query = "near")
  expect_true("role" %in% colnames(tab_q))

  # iterative_recommender (non-interactive mode)
  res <- iterative_recommender(fc, initial = "near", max_rounds = 2, verbose = FALSE)
  expect_true(inherits(res$query, "Set"))
  expect_true(inherits(res$implications, "ImplicationSet"))
})
