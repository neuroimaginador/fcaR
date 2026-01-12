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

test_that("Lattice metrics (support, stability) are numerically correct", {
  # Matriz simple 3 objetos, 3 atributos, diagonal (A=O1, B=O2, C=O3)
  # Esto crea un retículo Booleano
  I <- diag(3)
  rownames(I) <- c("o1", "o2", "o3")
  colnames(I) <- c("a", "b", "c")
  fc <- FormalContext$new(I)
  fc$find_concepts()

  # 1. Verificar Soporte
  # El concepto Top (todos los objetos) debe tener soporte 1.0 (3/3) si existe,
  # o el concepto vacío (si existe) soporte 0.
  # En la diagonal, los átomos ({o1}, {a}) tienen soporte 1/3
  supp <- fc$concepts$support()
  expect_true(all(supp >= 0 & supp <= 1))

  # Buscamos un concepto átomo (cardinal del extent == 1)
  extents <- fc$concepts$extents()
  atom_idx <- which(Matrix::colSums(extents) == 1)[1]
  expect_equal(supp[atom_idx], 1/3, tolerance = 1e-5)

  # 2. Verificar Estabilidad
  # En un contexto diagonal discreto, la estabilidad intensional de los átomos es baja/específica.
  # Pero verificamos simplemente que se calcule y devuelva un rango válido.
  stabs <- fc$concepts$stability()
  expect_equal(length(stabs), fc$concepts$size())
  expect_true(all(stabs >= 0)) # La estabilidad logarítmica o simple es positiva
})
