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

expect_error(fc$extract_implications_concepts(), NA)

test_that("fcaR creates a ConceptLattice", {

  expect_is(fc$concepts, "ConceptLattice")

})

test_that("fcaR plots a ConceptLattice", {

  expect_error(fc$concepts$plot(), NA)

})

test_that("fcaR prints a ConceptLattice", {

  expect_output(fc$concepts$print())

})

test_that("fcaR writes a ConceptLattice to LaTeX", {

  expect_error(fc$concepts$to_latex(), NA)
  expect_error(fc$concepts$to_latex(numbered = FALSE, align = FALSE), NA)
  expect_error(fc$concepts$to_latex(ncols = 2), NA)

})

test_that("fcaR extracts concepts from a ConceptLattice", {

  expect_error(L <- fc$concepts[10:12], NA)
  expect_is(L, "list")
  expect_is(L[[1]], "SparseConcept")

})

test_that("fcaR computes the sublattice of a ConceptLattice", {

  L <- fc$concepts[10:12]

  expect_error(cl <- fc$concepts$sublattice(10:13), NA)
  expect_is(cl, "ConceptLattice")

  expect_error(cl <- fc$concepts$sublattice(L), NA)
  expect_is(cl, "ConceptLattice")

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

  expect_error(fc$concepts$subconcepts(L[[3]]), NA)
  expect_error(fc$concepts$superconcepts(L[[3]]), NA)

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
