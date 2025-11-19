test_that("Lattice plotting runs without error", {
  fc <- FormalContext$new(planets)
  fc$find_concepts()

  # Probar modo Sugiyama
  expect_error(fc$concepts$plot(mode = "reduced", method = "sugiyama"), NA)

  # Probar modo Force
  expect_error(fc$concepts$plot(mode = "empty", method = "force"), NA)

  # Probar exportación a LaTeX (debe devolver objeto tikz_code)
  res <- fc$concepts$plot(to_latex = TRUE)
  expect_s3_class(res, "tikz_code")
})
