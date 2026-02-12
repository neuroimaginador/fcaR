context("Matrix Factorization Advanced")

test_that("Factorization works for fuzzy contexts and recovers dimensions", {
  # Crear un contexto difuso
  I_fuzzy <- matrix(
    c(0.9, 0.1, 0.8, 0.2, 1.0, 0.0, 0.8, 0.0, 0.9),
    nrow = 3,
    byrow = TRUE
  )
  colnames(I_fuzzy) <- c("a", "b", "c")
  rownames(I_fuzzy) <- c("o1", "o2", "o3")

  fc <- FormalContext$new(I_fuzzy)

  # 1. Probar GreConD con parámetros específicos
  # Usamos una lógica específica si está disponible, sino por defecto
  fc$use_logic("Lukasiewicz")
  res_grecond <- fc$factorize(
    method = "GreConD",
    w = 0.5,
    stop_threshold_ratio = 0.1
  )

  expect_is(res_grecond$object_factor, "FormalContext")
  expect_is(res_grecond$factor_attribute, "FormalContext")

  # Verificar dimensiones del producto (k factores)
  # A es (n_obj x k), B es (k x n_attr)
  expect_equal(res_grecond$object_factor$dim()[1], 3) # n_objects
  expect_equal(res_grecond$factor_attribute$dim()[2], 3) # n_attributes
  expect_equal(
    res_grecond$object_factor$dim()[2],
    res_grecond$factor_attribute$dim()[1]
  ) # k == k

  # 2. Probar ASSO con umbral alto (debe generar factores)
  res_asso <- fc$factorize(method = "ASSO", threshold = 0.6, w_pos = 1.5)
  expect_false(is.null(res_asso))
  expect_is(res_asso$object_factor, "FormalContext")

  # 3. Manejo de errores en métodos desconocidos
  expect_error(
    fc$factorize(method = "UnknownMethod"),
    "Unknown factorization method"
  )

  # 4. Factorización de contexto vacío
  fc_empty <- FormalContext$new(matrix(0, nrow = 2, ncol = 2))
  # GreConD podría devolver NULL o factores vacíos dependiendo de la implementación interna exacta
  # pero verificamos que no crashee
  expect_warning(fc_empty$factorize(method = "GreConD"), "No factors found.")
})
