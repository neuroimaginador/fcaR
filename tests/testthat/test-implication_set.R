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

  expect_error(ImplicationSet$new()$to_arules())

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

  imps <- ImplicationSet$new(attributes = letters[1:3])
  expect_error(LHS <- imps$get_LHS_matrix(), NA)
  expect_equal(dim(LHS), c(3, 1))
  expect_error(RHS <- imps$get_RHS_matrix(), NA)
  expect_equal(dim(RHS), c(3, 1))

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
  expect_error(cl <- fc$implications$closure(A, reduce = FALSE, verbose = TRUE), NA)

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
  expect_error(fc$implications$filter(lhs = fc$attributes[1],
                                      rhs = fc$attributes[1:2]), NA)

  expect_warning(fc$implications$filter(lhs = fc$attributes[6],
                                        rhs = fc$attributes[3]))

  expect_error(fc$implications$filter(rhs = fc$attributes[1]), NA)
  expect_error(fc$implications$filter(lhs = fc$attributes[1:2]), NA)
  expect_error(fc$implications$filter(rhs = fc$attributes[1],
                                             drop = TRUE), NA)

  expect_error(fc$implications$filter(not_rhs = fc$attributes[2]), NA)

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

test_that("fcaR combines implications", {

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
  expect_error(new_imps <- combine_implications(fc$implications[1:3], fc$implications[4:7]), NA)
  expect_equal(new_imps$cardinality(), 7)
  expect_error(new_imps <- combine_implications(fc$implications[0], fc$implications[0]), NA)
  expect_equal(new_imps$cardinality(), 0)


})


test_that("fcaR computes direct optimal basis using all C++ backends", {
  # Contexto pequeño pero suficiente para generar implicaciones no triviales
  I <- matrix(c(1, 0, 1, 0,
                1, 1, 0, 0,
                1, 1, 1, 0,
                0, 0, 1, 1), nrow = 4, byrow = TRUE)
  colnames(I) <- letters[1:4]
  rownames(I) <- paste0("O", 1:4)

  fc <- FormalContext$new(I)
  fc$find_implications()

  # Guardamos una copia de la base original para comparar equivalencia semántica
  base_imps <- fc$implications$clone()

  methods <- c("direct_optimal", "final_ts", "monotonic", "priority")

  for (m in methods) {
    # Clonamos para no alterar el original y probar cada método desde cero
    imps_test <- base_imps$clone()

    # Verificar que no da error la ejecución C++
    expect_error(imps_test$to_direct_optimal(method = m), NA)

    # Verificar que la estructura interna sigue siendo válida
    expect_is(imps_test, "ImplicationSet")
    expect_gt(imps_test$cardinality(), 0)

    # PRUEBA DE ORO: Equivalencia Semántica
    # El conjunto optimizado debe deducir exactamente lo mismo que el original
    # (imps_test %~% base_imps) comprueba si A implica B y B implica A
    expect_true(imps_test %~% base_imps,
                label = paste("Equivalence check failed for method:", m))
  }
})

test_that("fcaR filters implications robustly", {
  # Creamos implicaciones manualmente para tener control total
  # a -> b
  # b -> c
  # a, c -> d
  fc <- FormalContext$new(matrix(0, ncol = 4, nrow = 2))
  fc$attributes <- c("a", "b", "c", "d")
  fc$implications <- parse_implications(c("a -> b; b -> c; a,c -> d"))

  # 1. Filtrar por LHS que contiene "a" (debería recuperar la 1 y la 3)
  res_lhs <- fc$implications$filter(lhs = "a")
  expect_equal(res_lhs$cardinality(), 2)
  # Verificamos que 'b -> c' NO está
  LHS_mat <- res_lhs$get_LHS_matrix()
  expect_equal(unname(LHS_mat["b", ]), c(0, 0))

  # 2. Filtrar por RHS que contiene "c" (debería recuperar la 2: b->c)
  res_rhs <- fc$implications$filter(rhs = "c")
  expect_equal(res_rhs$cardinality(), 1)
  expect_equal(sum(res_rhs$get_LHS_matrix()["b", ]), 1)

  # 3. Filtrar intersección (LHS="a" AND RHS="d") -> (a,c -> d)
  res_both <- fc$implications$filter(lhs = "a", rhs = "d")
  expect_equal(res_both$cardinality(), 1)

  # 4. Caso borde: Filtrado que no produce resultados
  # (ninguna regla tiene 'd' en el antecedente)
  # Esto suele devolver NULL o un ImplicationSet vacío, verificamos que no rompa
  expect_warning(empty_res <- fc$implications$filter(lhs = "d"), "No combination")
  expect_null(empty_res)
})

test_that("fcaR applies simplification rules with reordering and parallelization", {
  # Usamos un dataset real pequeño para esto
  data("planets", package = "fcaR")
  fc <- FormalContext$new(planets)
  fc$find_implications()

  # Línea base
  n_original <- fc$implications$cardinality()

  # Copia para testear reorder = TRUE (aleatoriedad en el orden de aplicación)
  imps_reorder <- fc$implications$clone()
  expect_error(imps_reorder$apply_rules(rules = c("composition", "simplification"),
                                        reorder = TRUE), NA)

  # El resultado debe seguir siendo equivalente semánticamente al original
  # (aunque el número de reglas pueda variar ligeramente o ser el mismo, la lógica no cambia)
  expect_true(imps_reorder %~% fc$implications)

  # Copia para testear batching (forzando batch_size pequeño)
  imps_batch <- fc$implications$clone()
  # Batch size 2 fuerza a que se procese en trozos
  expect_error(imps_batch$apply_rules(batch_size = 2L), NA)
  expect_true(imps_batch %~% fc$implications)
})

