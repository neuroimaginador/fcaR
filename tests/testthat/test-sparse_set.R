context("Set")

test_that("fcaR operates on sparse sets", {

  A <- matrix(1, nrow = 10, ncol = 1)
  B <- matrix(1, nrow = 10, ncol = 5)

  expect_error(print(A), NA)

  expect_error(.difference2(A, B), NA)
  expect_error(.difference2(B, B), NA)
  expect_error(.difference2(B, A), NA)

  expect_error(.union(A, B), NA)
  expect_error(.union(B, B), NA)
  expect_error(.union(B, A), NA)

})

test_that("fcaR operates on sparse sets II", {

  A <- Matrix::Matrix(c(FALSE, TRUE, FALSE), nrow = 3, ncol = 1)
  B <- matrix(1, nrow = 3, ncol = 5)

  expect_error(.difference2(A, B), NA)
  expect_error(.difference2(B, B), NA)
  expect_error(.difference2(B, A), NA)

  expect_error(.union(A, B), NA)
  expect_error(.union(B, B), NA)
  expect_error(.union(B, A), NA)

})

test_that("fcaR uses class Set", {

  attributes <- paste0("P", 1:6)

  expect_error(A <- Set$new(attributes = attributes), NA)
  expect_error(A$assign(attributes = "P1", values = 0.3), NA)

  expect_error(A["P1"], NA)
  expect_is(A["P1"], "Set")
  expect_equal(A["P1"]$cardinal(), 0.3)

  expect_is(A$get_vector(), "Matrix")

  expect_equal(A$get_attributes(), attributes)

  expect_equal(A$length(), 6)

  expect_output(A$print())

  expect_is(A$to_latex(FALSE), "character")
  expect_is(capture_output(A$to_latex(TRUE)), "character")

  expect_true(A %<=% A)

  expect_error(v <- as_vector(A), NA)
  expect_error(A2 <- as_Set(v), NA)

  expect_true(A %==% A2)

})

test_that("fcaR computes operations of Sets", {

  attributes <- paste0("P", 1:6)

  expect_error(A <- Set$new(attributes = attributes), NA)
  expect_error(A$assign(attributes = "P1", values = 0.5), NA)

  expect_error(B <- Set$new(attributes = attributes), NA)
  expect_error(B$assign(attributes = "P1", values = 0.3), NA)

  expect_error(A %-% B, NA)
  expect_error(B %-% A, NA)

  # Build two sparse sets
  S <- Set$new(attributes = c("A", "B", "C"))
  S$assign(A = 1, B = 1)
  T <- Set$new(attributes = c("A", "B", "C"))
  T$assign(A = 1, C = 1)

  # Intersection
  expect_error(S %&% T, NA)

  # Build two sparse sets
  S <- Set$new(attributes = c("A", "B", "C"))
  S$assign(A = 1, B = 1)
  T <- Set$new(attributes = c("A", "B", "C"))
  T$assign(C = 1)

  # Union
  expect_error(S %|% T, NA)

})

test_that("SparseSet handles numeric/logical indexing and empty sets", {
  attributes <- c("a", "b", "c")
  S <- Set$new(attributes = attributes)
  S$assign(a = 1, c = 0.5)

  # 1. Indexado Numérico
  expect_equal(S[1]$cardinal(), 1)
  expect_equal(S[2]$cardinal(), 0)

  # 2. Indexado Lógico
  # Seleccionar 'a' y 'c'
  mask <- c(TRUE, FALSE, TRUE)
  subS <- S[mask]
  expect_equal(subS$length(), 3) # Mantiene la dimensión original pero hace 0 los no seleccionados
  expect_equal(subS$cardinal(), 1.5)

  # 3. Comportamiento del Conjunto Vacío
  E <- Set$new(attributes = attributes)
  expect_equal(E$cardinal(), 0)
  # Verificar que el print no falla para conjunto vacío
  expect_output(E$print(), "\\{\\}")
  expect_match(E$to_latex(print = FALSE), "varnothing")
})
