context("ConceptLattice")

objects <- paste0("O", 1:6)
n_objects <- length(objects)

attributes <- paste0("P", 1:6)
n_attributes <- length(attributes)

I <- matrix(
  data = c(
    0, 1, 0.5, 0, 0, 0.5,
    0, 1, 0.5, 0, 0, 0.5,
    0.5, 1, 0, 0, 1, 0,
    0.5, 0, 0, 1, 0.5, 0,
    1, 0, 0, 0.5, 0, 0,
    0, 0, 1, 0, 0, 1
  ),
  nrow = n_objects,
  byrow = FALSE
)

colnames(I) <- attributes
rownames(I) <- objects

fc <- FormalContext$new(I)

test_that("fcaR uses empty concept lattices", {
  expect_error(fc$concepts$print(), NA)
  expect_error(fc$concepts[1], NA)
})

expect_error(fc$find_concepts(), NA)

test_that("fcaR creates a ConceptLattice", {
  expect_is(fc$concepts, "ConceptLattice")
})

test_that("fcaR plots a ConceptLattice", {
  skip_on_cran()
  skip_if_not_installed("hasseDiagram")
  expect_error(fc$concepts$plot(), NA)
  fcaR_options("escape_" = FALSE)
  expect_error(fc$concepts$plot(object_names = FALSE), NA)
  expect_error(fc$concepts$plot(object_names = TRUE), NA)

  fcaR_options("reduced_lattice" = FALSE)
  expect_error(fc$concepts$plot(), NA)
  expect_error(fc$concepts$plot(object_names = FALSE), NA)
})


test_that("fcaR plots concept lattices in LaTeX", {
  skip_on_cran()
  skip_if_not_installed("tinytex")
  # expect_error(fc$concepts$plot(to_latex = TRUE), NA)
  # myfile <- tempfile(fileext = ".tex")
  # expect_error(file2 <- fc$concepts$plot(to_latex = TRUE,
  #                                        filename = myfile,
  #                                        caption = "Test",
  #                                        label = "fig:test",
  #                                        pointsize = 12), NA)
  # expect_equal(file2, myfile)
  #
  # expect_error(file2 <- fc$concepts$plot(to_latex = TRUE,
  #                                        object_names = TRUE,
  #                                        filename = myfile,
  #                                        caption = "Test",
  #                                        label = "fig:test",
  #                                        pointsize = 12), NA)
  # expect_equal(file2, myfile)
  #
  # fcaR_options("reduced_lattice" = FALSE)
  # expect_error(fc$concepts$plot(to_latex = TRUE), NA)
  # myfile <- tempfile(fileext = ".tex")
  # expect_error(file2 <- fc$concepts$plot(to_latex = TRUE,
  #                                        filename = myfile,
  #                                        caption = "Test",
  #                                        label = "fig:test",
  #                                        pointsize = 12), NA)
  # expect_equal(file2, myfile)
  #
  # expect_error(file2 <- fc$concepts$plot(to_latex = TRUE,
  #                                        object_names = TRUE,
  #                                        filename = myfile,
  #                                        caption = "Test",
  #                                        label = "fig:test",
  #                                        pointsize = 12), NA)
  # expect_equal(file2, myfile)
})

test_that("fcaR prints a ConceptLattice", {
  expect_output(fc$concepts$print())
})

test_that("fcaR writes a ConceptLattice to LaTeX", {
  expect_error(fc$concepts$to_latex(), NA)
  expect_error(fc$concepts$to_latex(numbered = FALSE, align = FALSE), NA)
  expect_error(fc$concepts$to_latex(ncols = 2), NA)

  expect_error(fc$concepts[2]$to_latex(), NA)

  expect_error(fc$concepts[1:3]$to_latex(), NA)
})

test_that("fcaR extracts concepts from a ConceptLattice", {
  expect_error(L <- fc$concepts[10:12], NA)
  expect_is(L, "ConceptSet")
  expect_is(L$to_list()[[1]], "Concept")
  expect_error(fc$concepts[fc$concepts$support() > 0.5], NA)
  expect_error(fc$concepts$top(), NA)
  expect_error(fc$concepts$bottom(), NA)
})

test_that("fcaR computes the sublattice of a ConceptLattice", {
  L <- fc$concepts[10:12]

  expect_error(cl <- fc$concepts$sublattice(10:13), NA)
  expect_is(cl, "ConceptLattice")

  expect_error(cl <- fc$concepts$sublattice(L), NA)
  expect_is(cl, "ConceptLattice")

  L <- fc$concepts[10]

  expect_error(cl <- fc$concepts$sublattice(10), NA)
  expect_is(cl, "ConceptLattice")

  expect_error(cl <- fc$concepts$sublattice(L), NA)
  expect_is(cl, "ConceptLattice")

  expect_error(cl <- fc$concepts$sublattice(fc$concepts$sub(10)), NA)
  expect_is(cl, "ConceptLattice")

  expect_error(cl <- fc$concepts$sublattice(fc$concepts$support() > 0.1), NA)
  expect_is(cl, "ConceptLattice")

  expect_error(fc$concepts$sublattice(as.list(fc$concepts)), NA)
})

test_that("fcaR computes the join- and meet- irreducibles of a ConceptLattice", {
  expect_error(ji <- fc$concepts$join_irreducibles(), NA)
  expect_error(mi <- fc$concepts$meet_irreducibles(), NA)
})

test_that("fcaR computes the suprema and infima of sets of concepts", {
  L <- fc$concepts[10:12]

  expect_error(fc$concepts$supremum(L), NA)
  expect_error(fc$concepts$supremum(10:13), NA)
  expect_error(fc$concepts$infimum(L), NA)
  expect_error(fc$concepts$infimum(10:12), NA)
})

test_that("fcaR computes the subconcepts and superconcepts of a given concept", {
  L <- fc$concepts[10:12]

  expect_error(fc$concepts$subconcepts(L[3]), NA)
  expect_error(fc$concepts$superconcepts(L[3]), NA)
})

test_that("fcaR computes the support of concepts", {
  expect_error(fc$concepts$support(), NA)
  expect_error(fc$concepts$support(), NA)
})

test_that("fcaR computes the size of a ConceptLattice", {
  expect_error(fc$concepts$size(), NA)
  fc <- FormalContext$new(I)
  expect_error(sz <- fc$concepts$size(), NA)
  expect_equal(sz, 0)
})

test_that("fcaR finds the lower and upper neighbours of a concept", {
  C <- fc$concepts[2]
  expect_error(fc$concepts$lower_neighbours(C), NA)
  expect_error(fc$concepts$upper_neighbours(C), NA)
})


test_that("fcaR decomposes concepts in its meet-irreducible elements", {
  L <- fc$concepts[10:12]

  expect_error(cl <- fc$concepts$decompose(L), NA)
  expect_is(cl, "list")
  expect_is(cl[[1]], "ConceptSet")
})

test_that("fcaR calculates lattice properties correctly", {
  # CASO 1: Retículo Distributivo (Boolean Algebra)
  # La matriz identidad genera un retículo Booleano (2^n conceptos)
  I_dist <- 1 - diag(3)
  colnames(I_dist) <- c("a", "b", "c")
  rownames(I_dist) <- c("o1", "o2", "o3")
  fc_dist <- FormalContext$new(I_dist)
  fc_dist$find_concepts()

  # Propiedades esperadas para B3 (Boolean algebra of 3 atoms)
  expect_true(fc_dist$concepts$is_distributive(), label = "Boolean lattice should be distributive")
  expect_true(fc_dist$concepts$is_modular(), label = "Distributive implies modular")
  expect_true(fc_dist$concepts$is_semimodular())
  expect_true(fc_dist$concepts$is_atomic())

  # CASO 2: Retículo NO Distributivo (Pentágono N5 - contraejemplo clásico)
  # Contexto simple para generar N5:
  # Elementos: 0 < x < z < 1, y incomparable con x,z.
  # Matriz de incidencia manual para forzar estructura no modular
  I_n5 <- matrix(c(1, 1, 1, 1, 1,
                   0, 1, 1, 0, 1,
                   0, 0, 1, 0, 1,
                   0, 0, 0, 1, 1,
                   0, 0, 0, 0, 1), nrow = 5, byrow = TRUE)
  fc_n5 <- FormalContext$new(I_n5)
  fc_n5$find_concepts()

  # Verificamos que los métodos devuelven logical sin error
  prop_dist <- fc_n5$concepts$is_distributive()
  prop_mod  <- fc_n5$concepts$is_modular()

  expect_is(prop_dist, "logical")
  expect_is(prop_mod, "logical")

  expect_false(prop_dist)
  expect_false(prop_mod)

  # Cache check: Llamar por segunda vez debe devolver lo mismo (usa la cache privada)
  expect_equal(fc_n5$concepts$is_distributive(), prop_dist)
})

