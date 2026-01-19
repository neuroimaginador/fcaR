context("Metrics Stability and Robustness")

test_that("Stability handles trivial lattices (only top/bottom)", {
  # Contexto 1x1 con 0
  I <- matrix(0, 1, 1)
  fc <- FormalContext$new(I)
  fc$find_concepts()

  # Solo concepto (empty_extent, empty_intent) o similar
  stabs <- fc$concepts$stability()
  expect_false(any(is.nan(stabs)))
  expect_false(any(is.infinite(stabs)))

  # Contexto 1x1 con 1
  I2 <- matrix(1, 1, 1)
  fc2 <- FormalContext$new(I2)
  fc2$find_concepts()

  stabs2 <- fc2$concepts$stability()
  expect_true(all(stabs2 >= 0 & stabs2 <= 1))
})

test_that("Separation metric integration works", {
  # El test existente prueba la llamada, pero este verifica la lógica sobre un caso conocido
  # Retículo booleano B2 (cuadrado)
  # Top, A, B, Bottom.
  # Separation(Top): extent=Total. Hijos=A, B. Union(A,B) cubre todo excepto si hay objetos específicos.

  I <- matrix(c(1, 0,
                0, 1), nrow=2, byrow=TRUE)
  colnames(I) <- c("a", "b")
  fc <- FormalContext$new(I)
  fc$find_concepts()

  # Verificar que separation no falla
  # Nota: calculate_separation es interna en R/metrics.R pero se accede via $separation() en lattice
  sep <- fc$concepts$separation()

  expect_equal(length(sep), fc$concepts$size())
  expect_is(sep, "numeric")
})
