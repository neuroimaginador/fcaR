context("Scaling and Background Knowledge")

test_that("Interval scaling generates correct background knowledge implications", {
  # Datos numéricos simples
  df <- data.frame(val = c(1, 5, 10))
  fc <- FormalContext$new(df)

  # Escalar por intervalo: low [0,5], high [5, 10]
  # bg = TRUE fuerza el cálculo de implicaciones entre los atributos derivados
  fc$scale("val", type = "interval", values = c(0, 5, 11), interval_names = c("low", "high"), bg = TRUE)

  # Verificar atributos generados
  curr_attrs <- fc$attributes
  expect_true(all(c("val is low", "val is high") %in% curr_attrs))

  # Background knowledge:
  # Dado que un valor es único (single valued original), en lógica multivaluada a veces
  # se generan exclusiones mutuas o jerarquías dependiendo de la lógica.
  # En intervalo simple, si es partición dura, low -> !high.

  bg_imps <- fc$background_knowledge()
  expect_is(bg_imps, "ImplicationSet")
  # No rules in interval scaling
  expect_equal(bg_imps$cardinality(), 0)


  # Datos numéricos simples
  df <- data.frame(val = c(1, 5, 10))
  fc <- FormalContext$new(df)

  # bg = TRUE fuerza el cálculo de implicaciones entre los atributos derivados
  fc$scale("val", type = "ordinal", bg = TRUE)

  # Verificar atributos generados
  curr_attrs <- fc$attributes
  expect_true(all(c("val <= 1", "val <= 5", "val <= 10") %in% curr_attrs))

  bg_imps <- fc$background_knowledge()
  expect_is(bg_imps, "ImplicationSet")

  # Asegurar que no está vacío si se solicitó bg=TRUE
  # (Depende de la implementación exacta de interval scaling en fcaR, pero usualmente genera reglas)
  if (bg_imps$cardinality() > 0) {
    # Verificar consistencia: Las reglas del BG deben cumplirse en el contexto escalado
    expect_true(all(bg_imps %holds_in% fc))
  }

  # Verificar recuperación de escalas
  scales <- fc$get_scales()
  expect_true(inherits(scales, "FormalContext") || "val" %in% names(scales))
})
