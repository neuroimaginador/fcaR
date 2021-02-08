context("Formal Context")

if (requireNamespace("arules", quietly = TRUE)) {

  data("Mushroom", package = "arules")
  expect_warning(
    mush <- arules::apriori(Mushroom,
                            parameter = list(conf = 1,
                                             maxlen = 4)))

}

test_that("fcaR creates a formal context", {

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

  fc <- FormalContext$new()
  expect_is(fc, "FormalContext")
  expect_output(fc$print())

  fc <- FormalContext$new(I = I)

  expect_is(fc, "FormalContext")

  expect_equal(fc$dim(), c(n_objects, n_attributes))
  expect_output(fc$print())

  # Now, without names
  I <- matrix(data = c(0, 1, 0.5, 0, 0, 0.5,
                       1, 1, 0.5, 0, 0, 0,
                       0.5, 1, 0, 0, 1, 0,
                       0.5, 0, 0, 1, 0.5, 0,
                       1, 0, 0, 0.5, 0, 0,
                       0, 0, 0, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  fc <- FormalContext$new(I = I)

  expect_is(fc, "FormalContext")

  # Return the incidence matrix
  expect_true(all(fc$incidence() == I))

})

test_that("fcaR imports from CXT and CSV files", {

  # Read CSV
  filename <- system.file("contexts", "airlines.csv",
                          package = "fcaR")

  fc <- FormalContext$new(filename)
  expect_is(fc, "FormalContext")

  # Read CXT
  filename <- system.file("contexts", "lives_in_water.cxt",
                          package = "fcaR")

  fc <- FormalContext$new(filename)
  expect_is(fc, "FormalContext")

})

test_that("fcaR computes the dual formal context", {

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

  fc <- FormalContext$new()
  fc <- FormalContext$new(I)

  fc2 <- fc$dual()
  expect_is(fc2, "FormalContext")

  expect_equal(fc2$dim(), c(n_attributes, n_objects))
  expect_output(fc2$print())
  expect_equal(fc2$objects, fc$attributes)
  expect_equal(fc2$attributes, fc$objects)

})

test_that("fcaR imports a formal context with constant columns", {

  objects <- paste0("O", 1:6)
  n_objects <- length(objects)

  attributes <- paste0("P", 1:6)
  n_attributes <- length(attributes)

  I <- matrix(data = c(0, 1, 0.5, 0, 0, 0.5,
                       1, 1, 1, 1, 1, 1,
                       0.5, 1, 0, 0, 1, 0,
                       0.5, 0, 0, 1, 0.5, 0,
                       1, 0, 0, 0.5, 0, 0,
                       0, 0, 0, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  colnames(I) <- attributes
  rownames(I) <- objects

  fc <- FormalContext$new(I = I, remove_const = TRUE)

  expect_is(fc, "FormalContext")

})

test_that("fcaR exports FormalContexts to LaTeX", {

  objects <- paste0("O", 1:6)
  n_objects <- length(objects)

  attributes <- paste0("P", 1:6)
  n_attributes <- length(attributes)

  I <- matrix(data = c(0, 1, 0.5, 0, 0, 0.5,
                       1, 1, 1, 1, 1, 1,
                       0.5, 1, 0, 0, 1, 0,
                       0.5, 0, 0, 1, 0.5, 0,
                       1, 0, 0, 0.5, 0, 0,
                       0, 0, 0, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  colnames(I) <- attributes
  rownames(I) <- objects

  fc <- FormalContext$new(I)

  expect_error(fc$to_latex(fraction = "frac"), NA)

  fc2 <- FormalContext$new(planets)

  expect_error(fc2$to_latex(), NA)


})

test_that("fcaR extracts concepts", {

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

  fc$find_concepts(verbose = TRUE)

  expect_is(fc$concepts, "ConceptLattice")

})

test_that("fcaR extracts implications", {

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

  fc$find_implications(verbose = TRUE)

  expect_is(fc$implications, "ImplicationSet")

})

test_that("fcaR generate plots", {

  skip_on_cran()

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

  expect_error(fc$plot(), NA)
  # expect_error(fc$plot(to_latex = TRUE), NA)
  # expect_error(fc$plot(to_latex = TRUE,
  #                      filename = "./test.tex",
  #                      caption = "Test",
  #                      label = "fig:test",
  #                      pointsize = 12), NA)

  fc <- FormalContext$new()

  expect_error(fc$plot())


})

test_that("fcaR imports formal contexts from arules", {

  skip_if_not_installed("arules")

  fc <- FormalContext$new(I = Mushroom)

  expect_is(fc, "FormalContext")

})




test_that("fcaR exports formal contexts to arules transactions", {

  skip_if_not_installed("arules")

  fc <- FormalContext$new(I = Mushroom)

  expect_is(fc$to_transactions(), "transactions")

})



test_that("fcaR prints large formal contexts", {

  I <- matrix(data = sample(c(0, 1),
                            size = 400,
                            replace = TRUE),
              nrow = 20)
  colnames(I) <- paste0("ATT_", seq(ncol(I)))

  fc <- FormalContext$new(I)
  expect_error(fc$print(), NA)
  expect_output(fc$print())

})

test_that("fcaR saves and loads formal contexts", {

  filename <- tempfile(fileext = ".RDS")

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

  expect_error(fc$save(filename = filename), NA)
  fc$find_implications()

  expect_error(fc$save(filename = filename), NA)

  expect_error(fc2 <- FormalContext$new(), NA)
  expect_error(fc2$load(filename), NA)


})

test_that("fcaR computes intents, extents and closures of SparseSets", {

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

  c1 <- fc$concepts[2][[1]]
  expect_error(fc$extent(c1$get_intent()), NA)
  expect_error(fc$intent(c1$get_extent()), NA)
  expect_error(fc$closure(c1$get_intent()), NA)

  expect_error(fc$intent(c1$get_intent()))
  expect_error(fc$extent(c1$get_extent()))
  expect_error(fc$closure(c1$get_extent()))

})

test_that("fcaR checks for concepts", {

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
  fc$find_concepts()

  for (i in seq(fc$concepts$size())) {

    C <- fc$concepts[i][[1]]

    expect_error(fc$is_closed(C$get_intent()), NA)
    expect_error(fc$is_concept(C), NA)
    expect_error(fc$extent(C$get_intent()), NA)
    expect_error(fc$intent(C$get_extent()), NA)
    expect_error(fc$closure(C$get_intent()), NA)

  }


})

test_that("fcaR clarifies and reduces contexts", {

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

  expect_error(fc2 <- fc$clarify(TRUE), NA)
  expect_error(fc$clarify(), NA)
  expect_error(fc$reduce())

  I2 <- I
  I2[I2 > 0] <- 1

  colnames(I2) <- attributes
  rownames(I2) <- objects

  fc <- FormalContext$new(I2)

  expect_error(fc2 <- fc$reduce(TRUE), NA)
  expect_error(fc$reduce(), NA)

})

test_that("fcaR computes the standard context", {

  skip_on_os("solaris")

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

  expect_error(fc2 <- fc$standardize())

  expect_error(fc$find_implications(), NA)
  expect_error(fc2 <- fc$standardize(), NA)

  expect_is(fc2, "FormalContext")
  expect_error(fc2$find_implications(), NA)

  expect_equal(fc$concepts$size(), fc2$concepts$size())
  # expect_error(fc$clarify(), NA)

})

test_that("fcaR computes object and attribute concepts", {

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

  expect_error(fc$att_concept("P1"), NA)
  expect_error(fc$obj_concept("O3"), NA)

})
