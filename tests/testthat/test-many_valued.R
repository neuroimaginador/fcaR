# Nominal
A_nom <- matrix(c("yes", "yes", "no", "yes", "no"), ncol = 1)
colnames(A_nom) <- "Grant"
rownames(A_nom) <- paste0("Student ", seq(nrow(A_nom)))

# Ordinal
A_ord <- matrix(c(7, 10, 5, 8, 4), ncol = 1)
colnames(A_ord) <- "Intern"
rownames(A_ord) <- paste0("Student ", seq(nrow(A_ord)))

# Interordinal
A_int <- matrix(c("agree", "strongly agree", "neither agree nor disagree", "agree", "disagree"), ncol = 1)
colnames(A_int) <- "Agreement"
rownames(A_int) <- paste0("Student ", seq(nrow(A_int)))

# Biordinal
A_bi <- matrix(c("working", "hard working", "working", "working", "lazy"), ncol = 1)
colnames(A_bi) <- "Attitude"
rownames(A_bi) <- paste0("Student ", seq(nrow(A_bi)))

# Interval
A_interv <- matrix(c(2.7, 4.1, 3.6, 4, 3.6), ncol = 1)
colnames(A_interv) <- "Score"
rownames(A_interv) <- paste0("Student ", seq(nrow(A_interv)))

# Aposition
A <- data.frame(A_nom, A_ord, A_int, A_bi, A_interv)

test_that("fcaR operates on many-valued contexts", {

  fc <- FormalContext$new(A)

  filename <- tempfile(fileext = ".RDS")
  expect_error(fc$save(filename), NA)
  expect_error(fc2 <- FormalContext$new(filename), NA)

  expect_error(fc$print(), NA)

  ## ----echo = FALSE----------------------------------
  expect_error(fc$incidence(), NA)


  ## --------------------------------------------------
  expect_error(fc$scale("Grant", type = "nominal"), NA)
  expect_error(fc$get_scales("Score"), NA)


  ## --------------------------------------------------
  expect_error(fc$scale("Intern", type = "ordinal"), NA)


  ## --------------------------------------------------
  expect_error(fc$scale("Agreement",
                        type = "interordinal",
                        values = c("strongly disagree",
                                   "disagree",
                                   "neither agree nor disagree",
                                   "agree",
                                   "strongly agree")),
               NA)

  expect_error(fc$find_implications())


  ## --------------------------------------------------
  expect_error(fc$scale("Attitude",
                        type = "biordinal",
                        values_le = c("hard working", "working"),
                        values_ge = c("lazy", "very lazy")),
               NA)


  ## --------------------------------------------------
  expect_error(fc$scale("Score",
                        type = "interval",
                        values = c(2, 3, 4, 5),
                        interval_names = c("C", "B", "A")),
               NA)

  ## --------------------------------------------------
  expect_error(fc$get_scales(c("Grant", "Score")), NA)
  expect_error(fc$get_scales("Grant"), NA)

  expect_error(fc$find_implications(), NA)
  expect_error(fc$background_knowledge(), NA)
  expect_is(fc$background_knowledge(), "ImplicationSet")



})

test_that("fcaR scales matrices", {

  expect_error(m <- nominal_scaling(A[, "Grant"], col_name = "Grant"), NA)
  expect_is(m, "matrix")

  expect_error(m <- ordinal_scaling(A[, "Intern"], col_name = "Intern"), NA)
  expect_is(m, "matrix")

  expect_error(m <- interordinal_scaling(A[, "Agreement"],
                                         col_name = "Agreement",
                                         values = c("strongly disagree",
                                                    "disagree",
                                                    "neither agree nor disagree",
                                                    "agree",
                                                    "strongly agree")), NA)
  expect_is(m, "matrix")

  expect_error(m <- biordinal_scaling(A[, "Attitude"],
                                      col_name = "Attitude",
                                      values_le = c("hard working", "working"),
                                      values_ge = c("lazy", "very lazy")),
               NA)
  expect_is(m, "matrix")

  expect_error(m <- interval_scaling(A[, "Score"],
                                     col_name = "Score",
                                     values = c(2, 3, 4, 5),
                                     interval_names = c("C", "B", "A")),
               NA)
  expect_is(m, "matrix")

})
