context("ConceptLattice")

objects <- paste0("O", 1:6)
n_objects <- length(objects)

attributes <- paste0("P", 1:6)
n_attributes <- length(attributes)

I <- matrix(data = c(0, 1, 0.5, 0, 0, 0.5,
                     0, 1, 0.5, 0, 0, 0.5,
                     0.5, 1, 0, 0, 1, 0,
                     0.5, 0, 0, 1, 0.5, 0,
                     1, 0, 0, 0.5, 0, 0,
                     0, 0, 1, 0, 0, 1),
            nrow = n_objects,
            byrow = FALSE)

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

test_that("fcaR finds the lower and upper neighbours of a concept",
          {

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

