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


test_that("Lattice plotting handles all modes and methods", {
  fc <- FormalContext$new(planets)
  fc$find_concepts()

  # 1. Probar todos los modos de etiquetado
  modes <- c("reduced", "full", "attributes", "empty")

  for (m in modes) {
    # Usamos expect_error(..., NA) para asegurar que la generación del gráfico no falla
    expect_error(p <- fc$concepts$plot(mode = m, method = "sugiyama"), NA,
                 info = paste("Failed plotting with mode:", m))
    expect_s3_class(p, "ggplot")
  }

  # 2. Probar todos los algoritmos de layout
  expect_error(fc$concepts$plot(method = "force"), NA)

  # 3. Verificar advertencia en retículo vacío
  fc_empty <- FormalContext$new(matrix(c(0, 0, 0), 3, 1))
  # fc_empty$find_concepts()
  expect_warning(fc_empty$concepts$plot(), "No concepts")
})
