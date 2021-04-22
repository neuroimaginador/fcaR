context("ImplicationSet")

if (requireNamespace("arules", quietly = TRUE)) {

  data("Mushroom", package = "arules")
  expect_warning(
    mush <- arules::apriori(Mushroom,
                            parameter = list(conf = 1,
                                             maxlen = 4)))

  idx_redundant <- arules::is.redundant(mush)

  mush_clean <- mush[!idx_redundant]

}

test_that("fcaR operates on implications", {

  objects <- paste0("O", 1:6)
  n_objects <- length(objects)

  attributes <- paste0("P", 1:6)
  n_attributes <- length(attributes)

  I <- matrix(data = c(0, 1, 0.5, 0, 0, 0.5,
                       1, 1, 0.5, 0, 0, 0,
                       0.5, 1, 0, 0, 1, 0,
                       0.5, 0, 0, 1, 0.5, 0,
                       1, 0, 0, 0.5, 0, 0,
                       0, 0, 1, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  colnames(I) <- attributes
  rownames(I) <- objects

  fc <- FormalContext$new(I = I)

  expect_error(fc$implications$apply_rules("composition"), NA)

  fc$find_implications()

  # Cardinality
  # TODO: Check that cardinality is an integer
  expect_is(fc$implications$cardinality(), "integer")

  # Rule size
  expect_is(fc$implications$size(), "matrix")

  # Use composition to reduce the number of implications
  expect_error(fc$implications$apply_rules(rules = c("composition"),
                                           parallelize = FALSE), NA)
  expect_is(fc$implications, "ImplicationSet")

  # Simplification
  expect_error(fc$implications$apply_rules(rules = c("simplification"),
                                           parallelize = FALSE), NA)
  expect_is(fc$implications, "ImplicationSet")

  # At this moment, we're at a fixed point, but we could apply
  # some more rules if needed:
  # TODO: Revisar Rsimplification con ejemplo planets
  expect_error(fc$implications$apply_rules(rules = equivalencesRegistry$get_entry_names(),
                                           reorder = TRUE,
                                           parallelize = FALSE), NA)
  expect_is(fc$implications, "ImplicationSet")

})

test_that("fcaR prints implications", {

  objects <- paste0("O", 1:6)
  n_objects <- length(objects)

  attributes <- paste0("P", 1:6)
  n_attributes <- length(attributes)

  I <- matrix(data = c(0, 1, 0.5, 0, 0, 0.5,
                       1, 1, 0.5, 0, 0, 0,
                       0.5, 1, 0, 0, 1, 0,
                       0.5, 0, 0, 1, 0.5, 0,
                       1, 0, 0, 0.5, 0, 0,
                       0, 0, 1, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  colnames(I) <- attributes
  rownames(I) <- objects

  fc <- FormalContext$new(I = I)
  fc$find_implications()

  expect_error(fc$implications[1:10]$print(), NA)
  expect_output(fc$implications[1:10]$print())

})

test_that("fcaR checks if implications hold in a context", {

  objects <- paste0("O", 1:6)
  n_objects <- length(objects)

  attributes <- paste0("P", 1:6)
  n_attributes <- length(attributes)

  I <- matrix(data = c(0, 1, 0.5, 0, 0, 0.5,
                       1, 1, 0.5, 0, 0, 0,
                       0.5, 1, 0, 0, 1, 0,
                       0.5, 0, 0, 1, 0.5, 0,
                       1, 0, 0, 0.5, 0, 0,
                       0, 0, 1, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  colnames(I) <- attributes
  rownames(I) <- objects

  fc <- FormalContext$new(I = I)
  fc$find_implications()
  imps <- fc$implications$clone()
  expect_true(all(imps %holds_in% fc))
  expect_true(all(Matrix::as.matrix(fc %respects% imps)))

})

test_that("fcaR checks entailment and equivalence of implication sets", {

  fc_planets <- FormalContext$new(planets)
  fc_planets$find_implications()
  # imps is the basis
  imps <- fc_planets$implications$clone()
  imps2 <- imps$clone()
  # imps2 is an equivalent set of implications
  # where we have removed redundancies
  imps2$apply_rules(c("simp"))
  # Any implication in imps2 follows from imps
  expect_true(all(imps %entails% imps2))
  # And viceversa
  expect_true(all(imps2 %entails% imps))

  # Equivalence of implication sets
  expect_true(imps %~% imps2)
  # If we remove any implication from imps2,
  # they will not be equivalent
  expect_false(imps %~% imps2[1:9])

})

test_that("fcaR adds and appends implications", {

  skip_if_not_installed("arules")

  fc <- FormalContext$new(I = Mushroom)

  # TODO: Falla cuando se añaden implicaciones a algo vacío
  fc$implications$add(mush_clean)

  fc$implications$add(fc$implications)
  first_lhs <- .extract_column(fc$implications$get_LHS_matrix(), 1)
  first_rhs <- .extract_column(fc$implications$get_RHS_matrix(), 1)

  expect_error(fc$implications$add(first_lhs, first_rhs), NA)

})

test_that("fcaR imports implications from arules", {

  skip_if_not_installed("arules")

  fc <- FormalContext$new(I = Mushroom)
  fc$implications$add(mush_clean)
  expect_is(fc$implications, "ImplicationSet")

  imps <- fc$implications$clone()
  fc$implications$add(imps)
  expect_is(fc$implications, "ImplicationSet")

  expect_error(fc$implications$add(1, 2, 3))

})

test_that("fcaR exports implications to arules", {

  skip_if_not_installed("arules")

  fc <- FormalContext$new()
  expect_error(fc$implications$to_arules())

  fc <- FormalContext$new(I = Mushroom)

  fc$implications$add(mush_clean)

  fc$implications$apply_rules("composition", parallelize = FALSE)

  expect_error(my_rules <- fc$implications$to_arules(quality = TRUE), NA)

  expect_is(my_rules, "rules")

  # With fuzzy context:
  objects <- paste0("O", 1:6)
  n_objects <- length(objects)

  attributes <- paste0("P", 1:6)
  n_attributes <- length(attributes)

  I <- matrix(data = c(0, 1, 0.5, 0, 0, 0.5,
                       1, 1, 0.5, 0, 0, 0,
                       0.5, 1, 0, 0, 1, 0,
                       0.5, 0, 0, 1, 0.5, 0,
                       1, 0, 0, 0.5, 0, 0,
                       0, 0, 1, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  colnames(I) <- attributes
  rownames(I) <- objects

  fc <- FormalContext$new(I = I)
  fc$find_implications()

  expect_error(fc$implications$to_arules())

})

test_that("fcaR computes implication support", {

  objects <- paste0("O", 1:6)
  n_objects <- length(objects)

  attributes <- paste0("P", 1:6)
  n_attributes <- length(attributes)

  I <- matrix(data = c(0, 1, 0.5, 0, 0, 0.5,
                       1, 1, 0.5, 0, 0, 0,
                       0.5, 1, 0, 0, 1, 0,
                       0.5, 0, 0, 1, 0.5, 0,
                       1, 0, 0, 0.5, 0, 0,
                       0, 0, 1, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  colnames(I) <- attributes
  rownames(I) <- objects

  fc <- FormalContext$new(I = I)
  expect_error(fc$implications$support(), NA)

  fc$find_implications()

  expect_error(fc$implications$support(), NA)
  expect_error(fc$implications$support(), NA)

})

test_that("fcaR exports implications to latex", {

  objects <- paste0("O", 1:6)
  n_objects <- length(objects)

  attributes <- paste0("P", 1:6)
  n_attributes <- length(attributes)

  I <- matrix(data = c(0, 1, 0.5, 0, 0, 0.5,
                       1, 1, 0.5, 0, 0, 0,
                       0.5, 1, 0, 0, 1, 0,
                       0.5, 0, 0, 1, 0.5, 0,
                       1, 0, 0, 0.5, 0, 0,
                       0, 0, 1, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  colnames(I) <- attributes
  rownames(I) <- objects

  fc <- FormalContext$new(I = I)
  fc$find_implications()

  expect_error(fc$implications[1:10]$to_latex(), NA)

})

test_that("fcaR gets LHS and RHS of implications", {

  objects <- paste0("O", 1:6)
  n_objects <- length(objects)

  attributes <- paste0("P", 1:6)
  n_attributes <- length(attributes)

  I <- matrix(data = c(0, 1, 0.5, 0, 0, 0.5,
                       1, 1, 0.5, 0, 0, 0,
                       0.5, 1, 0, 0, 1, 0,
                       0.5, 0, 0, 1, 0.5, 0,
                       1, 0, 0, 0.5, 0, 0,
                       0, 0, 1, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  colnames(I) <- attributes
  rownames(I) <- objects

  fc <- FormalContext$new(I = I)
  fc$find_implications()

  expect_is(fc$implications$get_LHS_matrix(), "dgCMatrix")
  expect_is(fc$implications$get_RHS_matrix(), "dgCMatrix")

})

test_that("fcaR computes closure wrt implications", {

  skip_if_not_installed("arules")

  fc <- FormalContext$new(I = Mushroom)

  fc$implications$add(mush_clean)

  # A fuzzy set
  A <- Set$new(attributes = fc$attributes)
  A$assign(attributes = "CapColor=white", values = 1)

  # Compute the closure
  expect_error(cl <- fc$implications$closure(A, reduce = TRUE, verbose = TRUE), NA)
  # Associated attributes
  expect_is(cl$closure, "Set")

  expect_is(cl$implications, "ImplicationSet")

})

test_that("fcaR simplifies implications", {

  objects <- paste0("O", 1:6)
  n_objects <- length(objects)

  attributes <- paste0("P", 1:6)
  n_attributes <- length(attributes)

  I <- matrix(data = c(0, 1, 0.5, 0, 0, 0.5,
                       1, 1, 0.5, 0, 0, 0,
                       0.5, 1, 0, 0, 1, 0,
                       0.5, 0, 0, 1, 0.5, 0,
                       1, 0, 0, 0.5, 0, 0,
                       0, 0, 1, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  colnames(I) <- attributes
  rownames(I) <- objects

  fc <- FormalContext$new(I = I)
  fc$find_implications()

  L <- .simplification(LHS = fc$implications$get_LHS_matrix(),
                       RHS = fc$implications$get_RHS_matrix(),
                       attributes = fc$attributes,
                       trace = TRUE)

  expect_is(L, "list")

  L <- Rsimplification(LHS = fc$implications$get_LHS_matrix(),
                          RHS = fc$implications$get_RHS_matrix(),
                          attributes = fc$attributes)

  expect_is(L, "list")


})

test_that("fcaR makes a recommendation", {

  objects <- paste0("O", 1:6)
  n_objects <- length(objects)

  attributes <- paste0("P", 1:6)
  n_attributes <- length(attributes)

  I <- matrix(data = c(0, 1, 0.5, 0, 0, 0.5,
                       1, 1, 0.5, 0, 0, 0,
                       0.5, 1, 0, 0, 1, 0,
                       0.5, 0, 0, 1, 0.5, 0,
                       1, 0, 0, 0.5, 0, 0,
                       0, 0, 1, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  colnames(I) <- attributes
  rownames(I) <- objects

  fc <- FormalContext$new(I = I)
  fc$find_implications()

  # A fuzzy set
  S <- Set$new(attributes = fc$attributes)
  S$assign(P1 = 1)

  expect_error(fc$implications$recommend(S = S, attribute_filter = fc$attributes[1]), NA)

})

test_that("fcaR filters and removes implications", {

  objects <- paste0("O", 1:6)
  n_objects <- length(objects)

  attributes <- paste0("P", 1:6)
  n_attributes <- length(attributes)

  I <- matrix(data = c(0, 1, 0.5, 0, 0, 0.5,
                       1, 1, 0.5, 0, 0, 0,
                       0.5, 1, 0, 0, 1, 0,
                       0.5, 0, 0, 1, 0.5, 0,
                       1, 0, 0, 0.5, 0, 0,
                       0, 0, 1, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  colnames(I) <- attributes
  rownames(I) <- objects

  fc <- FormalContext$new(I = I)
  fc$find_implications()

  # TODO: FALLA el filtrado
  expect_error(fc$implications$filter(lhs = fc$attributes[1], rhs = fc$attributes[1:2]), NA)

  expect_warning(fc$implications$filter(lhs = fc$attributes[6], rhs = fc$attributes[3]))

  expect_error(fc$implications$filter(rhs = fc$attributes[1]), NA)
  expect_error(fc$implications$filter(lhs = fc$attributes[1:2]), NA)
  expect_error(fc$implications$filter(rhs = fc$attributes[1],
                                             drop = TRUE), NA)

  n <- fc$implications$cardinality()

  expect_error(imp2 <- fc$implications[-c(1:2)], NA)

  n2 <- imp2$cardinality()

  expect_equal(n2, n - 2)

})

test_that("fcaR adds implications from scratch", {

  objects <- paste0("O", 1:6)
  n_objects <- length(objects)

  attributes <- paste0("P", 1:6)
  n_attributes <- length(attributes)

  I <- matrix(data = c(0, 1, 0.5, 0, 0, 0.5,
                       1, 1, 0.5, 0, 0, 0,
                       0.5, 1, 0, 0, 1, 0,
                       0.5, 0, 0, 1, 0.5, 0,
                       1, 0, 0, 0.5, 0, 0,
                       0, 0, 1, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  colnames(I) <- attributes
  rownames(I) <- objects

  fc <- FormalContext$new(I = I)

  fc$implications <- ImplicationSet$new(attributes = fc$attributes)
  expect_equal(fc$implications$cardinality(), 0)

  expect_output(print(fc$implications))

  lhs1 <- Set$new(attributes = fc$attributes)
  lhs1$assign(attributes = fc$attributes[1],
              values = 1)

  rhs1 <- Set$new(attributes = fc$attributes)
  rhs1$assign(fc$attributes[c(2,4)],
              values = c(1, 1))

  expect_error(fc$implications$add(lhs = lhs1, rhs = rhs1), NA)

})

test_that("fcaR can use generalization", {

  objects <- paste0("O", 1:6)
  n_objects <- length(objects)

  attributes <- paste0("P", 1:6)
  n_attributes <- length(attributes)

  I <- matrix(data = c(0, 1, 0.5, 0, 0, 0.5,
                       1, 1, 0.5, 0, 0, 0,
                       0.5, 1, 0, 0, 1, 0,
                       0.5, 0, 0, 1, 0.5, 0,
                       1, 0, 0, 0.5, 0, 0,
                       0, 0, 1, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  colnames(I) <- attributes
  rownames(I) <- objects

  fc <- FormalContext$new(I = I)

  fc$implications <- ImplicationSet$new(attributes = fc$attributes)
  expect_equal(fc$implications$cardinality(), 0)

  lhs1 <- Set$new(attributes = fc$attributes)
  lhs1$assign(attributes = fc$attributes[1],
              values = 1)

  rhs1 <- Set$new(attributes = fc$attributes)
  rhs1$assign(fc$attributes[c(2,4)],
              values = c(1, 1))

  fc$implications$add(lhs1, rhs1)

  expect_error(fc$implications$apply_rules("composition"), NA)

  lhs2 <- Set$new(attributes = fc$attributes)
  lhs2$assign(attributes = fc$attributes[c(1, 3)],
              values = c(1, 1))

  rhs2 <- Set$new(attributes = fc$attributes)
  rhs2$assign(fc$attributes[4],
              values = 1)

  fc$implications$add(lhs2, rhs2)

  expect_error(fc$implications$apply_rules(rules = "generalization", parallelize = FALSE), NA)

})

test_that("fcaR filters implications", {

  objects <- paste0("O", 1:6)
  n_objects <- length(objects)

  attributes <- paste0("P", 1:6)
  n_attributes <- length(attributes)

  I <- matrix(data = c(0, 1, 0.5, 0, 0, 0.5,
                       1, 1, 0.5, 0, 0, 0,
                       0.5, 1, 0, 0, 1, 0,
                       0.5, 0, 0, 1, 0.5, 0,
                       1, 0, 0, 0.5, 0, 0,
                       0, 0, 1, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  colnames(I) <- attributes
  rownames(I) <- objects

  fc <- FormalContext$new(I = I)

  fc$implications <- ImplicationSet$new(attributes = fc$attributes)
  expect_equal(fc$implications$cardinality(), 0)

  lhs1 <- Set$new(attributes = fc$attributes)
  lhs1$assign(attributes = fc$attributes[1],
              values = 1)

  rhs1 <- Set$new(attributes = fc$attributes)
  rhs1$assign(fc$attributes[c(2,4)],
              values = c(1, 1))

  fc$implications$add(lhs1, rhs1)

  expect_warning(fc$implications$filter(lhs = fc$attributes[5]))

})

test_that("fcaR subsets implications", {

  objects <- paste0("O", 1:6)
  n_objects <- length(objects)

  attributes <- paste0("P", 1:6)
  n_attributes <- length(attributes)

  I <- matrix(data = c(0, 1, 0.5, 0, 0, 0.5,
                       1, 1, 0.5, 0, 0, 0,
                       0.5, 1, 0, 0, 1, 0,
                       0.5, 0, 0, 1, 0.5, 0,
                       1, 0, 0, 0.5, 0, 0,
                       0, 0, 1, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  colnames(I) <- attributes
  rownames(I) <- objects

  fc <- FormalContext$new(I = I)
  fc$find_implications()

  expect_error(fc$implications[fc$implications$support() > 0.1], NA)
  # TODO: FALLA
  expect_error(fc$implications[-c(1:2)], NA)
  expect_error(fc$implications[c(-1, 2)])
  expect_error(fc$implications[0], NA)

})

test_that("fcaR computes the canonical basis from an ImplicationSet", {

  objects <- paste0("O", 1:6)
  n_objects <- length(objects)

  attributes <- paste0("P", 1:6)
  n_attributes <- length(attributes)

  I <- matrix(data = c(0, 1, 0.5, 0, 0, 0.5,
                       1, 1, 0.5, 0, 0, 0,
                       0.5, 1, 0, 0, 1, 0,
                       0.5, 0, 0, 1, 0.5, 0,
                       1, 0, 0, 0.5, 0, 0,
                       0, 0, 1, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  colnames(I) <- attributes
  rownames(I) <- objects

  fc <- FormalContext$new(I = I)
  fc$find_implications()

  expect_error(imps <- fc$implications$to_basis(), NA)

  expect_is(imps, "ImplicationSet")

})

test_that(desc = "fcaR can use equivalence rules", {

  expect_error(rules <- equivalencesRegistry$get_entry_names(),
               NA)

  for (r in rules) {

    objects <- paste0("O", 1:6)
    n_objects <- length(objects)

    attributes <- paste0("P", 1:6)
    n_attributes <- length(attributes)

    I <- matrix(data = c(0, 1, 0.5, 0, 0, 0.5,
                         1, 1, 0.5, 0, 0, 0,
                         0.5, 1, 0, 0, 1, 0,
                         0.5, 0, 0, 1, 0.5, 0,
                         1, 0, 0, 0.5, 0, 0,
                         0, 0, 1, 0, 0, 0),
                nrow = n_objects,
                byrow = FALSE)

    colnames(I) <- attributes
    rownames(I) <- objects

    fc <- FormalContext$new(I = I)
    fc$find_implications()

    cat("Testing", r, "\n")

    expect_error(fc$implications$apply_rules(r,
                                             parallelize = FALSE),
                 NA)

  }

})
