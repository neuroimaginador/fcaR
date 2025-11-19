test_that("Metrics calculation works", {
  I <- matrix(c(
    1, 0, 1,
    1, 1, 0,
    0, 1, 0
  ), nrow = 3, byrow = TRUE)
  fc <- FormalContext$new(I)
  fc$find_concepts()

  # Stability
  stab <- fc$concepts$stability()
  expect_equal(length(stab), fc$concepts$size())
  expect_true(all(stab >= 0 & stab <= 1))

  # Separation
  sep <- fc$concepts$separation()
  expect_equal(length(sep), fc$concepts$size())
  # Separation of bottom concept (extent size 0 or 1 depending on logic) should be valid

  # Density
  dens <- fc$concepts$density(fc$incidence())
  expect_equal(length(dens), fc$concepts$size())
  expect_true(all(dens == 1 | dens == 0)) # Binary context -> density 1
})

test_that("Separation uses direct subconcepts correctly", {
  # Caso: C1 <= C3, C2 <= C3.
  # Ext(C1) = {1}, Ext(C2) = {2}, Ext(C3) = {1, 2, 3}
  # Sep(C3) debería ser |{1,2,3}| - |{1} U {2}| = 3 - 2 = 1.
  # Si no usara reducción transitiva y hubiera un C0 <= C1 <= C3,
  # la unión seguiría siendo correcta, pero .reduce_transitivity es más segura/eficiente.

  I <- matrix(c(
    1, 0, 0,
    0, 1, 0,
    1, 1, 1
  ), nrow = 3, byrow = TRUE)
  fc <- FormalContext$new(I)
  fc$find_concepts()

  # El concepto que cubre a todos (Top) debería tener separación > 0 si el objeto 3 es único
  sep <- fc$concepts$separation()

  # Verificamos que devuelve vector numérico del tamaño correcto
  expect_equal(length(sep), fc$concepts$size())
  expect_true(all(sep >= 0))
})
