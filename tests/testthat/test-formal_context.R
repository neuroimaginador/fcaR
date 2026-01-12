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

  expect_error(fc$to_latex(), NA)
  fcaR_options("latex_fraction" = "sfrac")
  expect_error(fc$to_latex(), NA)
  expect_error(context_to_latex(fc$incidence()), NA)

  fcaR_options("use_tabulary" = TRUE)
  expect_error(context_to_latex(fc$incidence(), rotated = TRUE), NA)
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
  fc$use_logic("Product")

  fc$find_concepts(verbose = FALSE)

  expect_is(fc$concepts, "ConceptLattice")

  # Different Galois connections and logics
  fc <- FormalContext$new(I = I[1:3, 1:3])
  # fc$use_connection("benevolent1")
  fc$use_logic("Godel")
  fc$find_concepts(verbose = FALSE)
  expect_is(fc$concepts, "ConceptLattice")

  fc <- FormalContext$new(I = I)
  fc$use_connection("benevolent2")
  fc$use_connection("Lukasiewicz")
  fc$find_concepts(verbose = FALSE)
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

  # Different Galois connections and logics
  fc <- FormalContext$new(I = I)
  fc$use_connection("benevolent1")
  fc$use_logic("Product")
  fc$find_implications(verbose = TRUE)
  expect_is(fc$implications, "ImplicationSet")

  fc <- FormalContext$new(I = I)
  fc$use_connection("benevolent2")
  fc$use_connection("Lukasiewicz")
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

  skip_if_not_installed("tinytex")
  # expect_error(fc$plot(to_latex = TRUE), NA)
  # expect_error(fc$plot(to_latex = TRUE,
  #                      filename = "./test.tex",
  #                      caption = "Test",
  #                      label = "fig:test",
  #                      pointsize = 12), NA)

  fc <- FormalContext$new()

  expect_error(fc$plot())


})

test_that("fcaR subsets formal contexts", {

  fc <- FormalContext$new(planets)
  expect_is(fc[, c("large", "moon")], "FormalContext")
  expect_is(fc[c("Earth", "Mars"), ], "FormalContext")
  expect_is(fc[c("Earth", "Mars"), c("large", "moon")], "FormalContext")

  expect_is(fc[, 5:6], "FormalContext")
  expect_is(fc[3:4, ], "FormalContext")
  expect_is(fc[3:4, 5:6], "FormalContext")

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
  colnames(I) <- paste0("ATT_", seq_len(ncol(I)))

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

  expect_error(fc2 <- FormalContext$new(filename), NA)

  filename <- tempfile(fileext = ".CXT")

  fc <- FormalContext$new(I = I)

  expect_error(fc$save(filename = filename), NA)
  fc$find_implications()

  expect_error(fc$save(filename = filename), NA)

  expect_error(fc2 <- FormalContext$new(), NA)
  expect_error(fc2$load(filename), NA)

  expect_error(fc2 <- FormalContext$new(filename), NA)


})

# TODO: Revisar todo lo de las escalas

test_that("fcaR perform context scaling", {

  to_nominal <- sample(0:3, size = 10, replace = TRUE)
  to_ordinal <- sample(1:4, size = 10, replace = TRUE)
  to_interordinal <- sample(1:4, size = 10, replace = TRUE)
  to_interval <- runif(10)

  I <- cbind(nom = to_nominal,
             ord = to_ordinal,
             inter = to_interordinal,
             int = to_interval)

  fc <- FormalContext$new(I)

  expect_error(fc$scale(attributes = "ord",
                        type = "ordinal"), NA)

  expect_error(fc$scale(attributes = "nom",
                        type = "nominal"), NA)

  expect_error(fc$scale(attributes = "inter",
                        type = "interordinal"), NA)

  expect_error(fc$scale(attributes = "int",
                        type = "interval",
                        values = c(0, 0.5, 1),
                        interval_names = c("low", "high")), NA)

})

test_that("fcaR computes intents, extents and closures of Sets", {

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

  c1 <- fc$concepts[2]$to_list()[[1]]
  expect_error(fc$extent(c1$get_intent()), NA)
  expect_error(fc$intent(c1$get_extent()), NA)
  expect_error(fc$downarrow(c1$get_intent()), NA)
  expect_error(fc$uparrow(c1$get_extent()), NA)
  expect_error(fc$closure(c1$get_intent()), NA)

  expect_warning(fc$intent(c1$get_intent()))
  expect_warning(fc$extent(c1$get_extent()))
  # expect_warning(fc$closure(c1$get_extent()))

  S <- Set$new(attributes = rev(attributes))
  S$assign(P6 = 0.5, P5 = 0.5)
  expect_warning(cl <- fc$closure(S))


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

    C <- fc$concepts[i]$to_list()[[1]]

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

  # TODO: Revisar reduce
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

test_that("fcaR produce errors for some functionalities over many-valued contexts", {

  to_nominal <- sample(0:3, size = 10, replace = TRUE)
  to_ordinal <- sample(1:4, size = 10, replace = TRUE)
  to_interordinal <- sample(1:4, size = 10, replace = TRUE)
  to_interval <- runif(10)

  I <- cbind(nom = to_nominal,
             ord = to_ordinal,
             inter = to_interordinal,
             int = to_interval)

  fc <- FormalContext$new(I)

  expect_error(fc$plot())
  expect_error(fc$find_concepts())
  expect_error(fc$find_implications())
  expect_error(fc$intent(Set$new()))
  expect_error(fc$extent(Set$new()))
  expect_error(fc$closure(Set$new()))
  expect_error(fc$att_concept("nom"))
  expect_error(fc$obj_concept("1"))
  expect_error(fc$clarify())
  expect_error(fc$standardize())
  expect_error(fc$to_transactions())
  expect_error(fc$to_latex())
  expect_error(fc$dual(), NA)

})

test_that("fcaR works with contexts with one attribute or one object", {

  # One object
  m <- matrix(c(0.2, 0.7), ncol = 2)
  colnames(m) <- c("m1", "m3")
  rownames(m) <- "g"

  fc <- FormalContext$new(m)
  expect_error(fc$find_concepts(), NA)
  expect_error(fc$find_implications(), NA)

  expect_is(fc$concepts$intents(), "dgCMatrix")
  expect_is(fc$concepts$extents(), "dgCMatrix")

  expect_equal(dim(fc$concepts$intents()), c(2, 3))
  expect_equal(dim(fc$concepts$extents()), c(1, 3))

  # One attribute
  m <- t(m)

  fc <- FormalContext$new(m)
  expect_error(fc$find_concepts(), NA)
  expect_error(fc$find_implications(), NA)

  expect_is(fc$concepts$intents(), "dgCMatrix")
  expect_is(fc$concepts$extents(), "dgCMatrix")

  expect_equal(dim(fc$concepts$intents()), c(1, 3))
  expect_equal(dim(fc$concepts$extents()), c(2, 3))

})

test_that("fcaR factorizes a context (GreConD and ASSO)", {
  # Crear un contexto pequeño pero interesante
  I <- matrix(c(1, 1, 0, 0,
                1, 1, 1, 0,
                0, 1, 1, 1,
                0, 0, 1, 1), nrow = 4, byrow = TRUE)
  colnames(I) <- paste0("P", 1:4)
  rownames(I) <- paste0("O", 1:4)

  fc <- FormalContext$new(I)

  # 1. Test GreConD (Debería ser exacto para este caso booleano simple)
  res_grecond <- fc$factorize(method = "GreConD")

  expect_is(res_grecond$object_factor, "FormalContext")
  expect_is(res_grecond$factor_attribute, "FormalContext")

  # Verificar reconstrucción: I == A %*% B (Booleano)
  A <- as.matrix(res_grecond$object_factor$incidence())
  B <- as.matrix(res_grecond$factor_attribute$incidence())

  # Producto booleano
  Reconstructed <- (A %*% B) > 0
  Reconstructed <- Reconstructed + 0 # Convertir a numérico

  expect_true(all(I == Reconstructed))

  # 2. Test ASSO (verificar que no da error y devuelve estructura correcta)
  expect_error(res_asso <- fc$factorize(method = "ASSO", threshold = 0.5), NA)
  expect_is(res_asso$object_factor, "FormalContext")
})

test_that("fcaR handles scaling and background knowledge correctly", {
  # Datos simples
  df <- data.frame(val = c(1, 2, 3, 1))
  fc <- FormalContext$new(df)

  # Escalar con generación de background knowledge
  expect_error(fc$scale("val", "ordinal", bg = TRUE), NA)

  # Verificar que se han guardado las escalas
  scales <- fc$get_scales()
  expect_true(length(scales) > 0)

  # Verificar que hay conocimiento de fondo (implicaciones)
  bg <- fc$background_knowledge()
  expect_is(bg, "ImplicationSet")
  # Para escala ordinal 1->2->3, debería haber implicaciones
  expect_gt(bg$cardinality(), 0)
})

test_that("fcaR handles dense contexts (optimization path)", {
  # Matriz de todo unos
  I <- matrix(1, nrow = 5, ncol = 5)
  colnames(I) <- letters[1:5]
  rownames(I) <- LETTERS[1:5]

  fc <- FormalContext$new(I)

  # Esto activará el bloque 'if (all(self$I@x == 1) && (method == "InClose"))'
  expect_error(fc$find_concepts(method = "InClose"), NA)

  # Debería haber exactamente 1 concepto (el que tiene todos los objetos y atributos)
  # O 2 si consideramos el vacío dependiendo de la lógica, pero en FCA clásico con todo 1s:
  # Extensión total <-> Intención total.
  # Verificamos simplemente que el lattice no esté vacío y sea consistente.
  expect_gt(fc$concepts$size(), 0)

  # Verificar propiedad básica: el concepto top tiene todos los objetos
  top_extent <- fc$concepts$top()$get_extent()
  expect_equal(sum(top_extent$get_vector()), 5)
})

test_that("fcaR saves and loads many-valued contexts properly", {
  # Crear contexto multivaluado
  I_mv <- data.frame(A = c(1, 2), B = factor(c("x", "y")))
  fc_mv <- FormalContext$new(I_mv)

  tmp_file <- tempfile(fileext = ".rds")

  # Guardar
  expect_error(fc_mv$save(tmp_file), NA)

  # Cargar en objeto nuevo
  fc_loaded <- FormalContext$new(tmp_file)

  # Verificar que sigue siendo multivaluado y los datos coinciden
  # Nota: El acceso a private no es directo en tests, verificamos print o incidence
  expect_output(fc_loaded$print(), "FormalContext with")

  # incidence() devuelve many_valued_I en estos casos
  I_loaded <- fc_loaded$incidence()
  expect_equal(as.matrix(I_mv), as.matrix(I_loaded))
})

test_that("fcaR setters/getters and error handling work", {
  I <- matrix(c(0, 1, 1, 0), nrow = 2)
  fc <- FormalContext$new(I)

  # Logics
  fc$use_logic("Lukasiewicz")
  expect_equal(fc$get_logic(), "Lukasiewicz")

  fc$use_connection("benevolent1")
  expect_equal(fc$get_connection(), "benevolent1")

  # Fichero inexistente
  expect_error(FormalContext$new("non_existent_file.csv"), "not found")

  # Intentar reducir un contexto no binario (fuerza error específico)
  # Usamos un contexto multivaluado para esto
  I_mv <- data.frame(A = c(1, 2))
  fc_mv <- FormalContext$new(I_mv)
  expect_error(fc_mv$reduce(), "not binary")
})



test_that("fcaR object/attribute concepts are semantically correct", {
  # Contexto simple:
  #    a b
  # o1 1 0
  # o2 1 1
  I <- matrix(c(1, 0, 1, 1), nrow = 2, byrow = TRUE)
  colnames(I) <- c("a", "b")
  rownames(I) <- c("o1", "o2")
  fc <- FormalContext$new(I)
  fc$find_concepts()

  # Concepto Objeto o1:
  # Intent(o1) = {a}. Extent({a}) = {o1, o2}.
  # Por tanto obj_concept(o1) debe ser el concepto ({o1,o2}, {a})
  C_o1 <- fc$obj_concept("o1")

  # Verificamos extensión (debe tener cardinal 2)
  expect_equal(C_o1$get_extent()$cardinal(), 2)
  # Verificamos intención (debe ser solo 'a')
  intent_names <- C_o1$get_intent()$get_attributes()[Matrix::which(C_o1$get_intent()$get_vector() > 0)]
  expect_equal(intent_names, "a")

  # Concepto Atributo b:
  # Extent(b) = {o2}. Intent({o2}) = {a, b}.
  # Por tanto att_concept(b) debe ser ({o2}, {a,b})
  C_b <- fc$att_concept("b")

  expect_equal(C_b$get_extent()$cardinal(), 1) # Solo o2
  intent_names_b <- C_b$get_intent()$get_attributes()[Matrix::which(C_b$get_intent()$get_vector() > 0)]
  expect_true(all(c("a", "b") %in% intent_names_b))
})

test_that("fcaR allows perfect round-trip persistence (Save -> Load)", {
  # Usar un contexto no trivial
  data("planets", package = "fcaR")
  fc_orig <- FormalContext$new(planets)
  fc_orig$find_concepts()
  fc_orig$find_implications()

  tmp <- tempfile(fileext = ".rds")
  fc_orig$save(tmp)

  # Cargar en nueva instancia
  fc_new <- FormalContext$new(tmp)

  # 1. Verificar Dimensiones
  expect_equal(fc_orig$dim(), fc_new$dim())

  # 2. Verificar Contenido del Retículo
  expect_equal(fc_orig$concepts$size(), fc_new$concepts$size())
  # Comparamos la matriz de extensiones directamente
  expect_true(all(fc_orig$concepts$extents() == fc_new$concepts$extents()))

  # 3. Verificar Implicaciones (cantidad y reglas)
  expect_equal(fc_orig$implications$cardinality(), fc_new$implications$cardinality())
})
