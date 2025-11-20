#' @importFrom methods slotNames
.subset <- function(x, y = NULL, proper = FALSE) {
  if (!inherits(x, "CsparseMatrix")) x <- as(x, "CsparseMatrix")
  if (is.null(y)) y <- x
  if (!inherits(y, "CsparseMatrix")) y <- as(y, "CsparseMatrix")

  proper_code <- if (proper) 1 else 2

  # --- DETECCIÓN DE TIPO ---
  is_binary <- inherits(x, "ngCMatrix") && inherits(y, "ngCMatrix")

  if (!is_binary) {
    if (!("x" %in% slotNames(x))) x <- as(x, "dgCMatrix")
    if (!("x" %in% slotNames(y))) y <- as(y, "dgCMatrix")
  }

  X_x <- if (is_binary) numeric(0) else x@x
  Y_x <- if (is_binary) numeric(0) else y@x

  res_list <- sparse_subset_dispatch(
    X_p = x@p, X_i = x@i, X_x = X_x,
    Y_p = y@p, Y_i = y@i, Y_x = Y_x,
    num_rows = x@Dim[1],
    proper_code = proper_code,
    is_binary = is_binary
  )

  # --- ¡LÓGICA CORREGIDA! ---
  # Usamos un if/else para construir la matriz correcta
  if (is_binary) {
    M <- Matrix::sparseMatrix(
      i = res_list$i,
      p = res_list$p,
      index1 = FALSE,
      dims = c(y@Dim[2], x@Dim[2])
      # Sin 'x' => ngCMatrix (lógica)
    )
  } else {
    M <- Matrix::sparseMatrix(
      i = res_list$i,
      p = res_list$p,
      index1 = FALSE,
      dims = c(y@Dim[2], x@Dim[2]),
      x = 1.0 # Con 'x' => dgCMatrix (numérica)
    )
  }
  return(as(Matrix::t(M), "nMatrix"))
}

# --- B. Wrapper .equal_sets (Detecta tipo) ---
.equal_sets <- function(x, y = NULL) {
  if (!inherits(x, "CsparseMatrix")) x <- as(x, "CsparseMatrix")
  if (is.null(y)) y <- x
  if (!inherits(y, "CsparseMatrix")) y <- as(y, "CsparseMatrix")

  is_binary <- inherits(x, "nMatrix") && inherits(y, "ngCMatrix")

  if (!is_binary) {
    if (!("x" %in% slotNames(x))) x <- as(x, "dgCMatrix")
    if (!("x" %in% slotNames(y))) y <- as(y, "dgCMatrix")
  }

  X_x <- if (is_binary) numeric(0) else x@x
  Y_x <- if (is_binary) numeric(0) else y@x

  res_list <- sparse_subset_dispatch(
    X_p = x@p, X_i = x@i, X_x = X_x,
    Y_p = y@p, Y_i = y@i, Y_x = Y_x,
    num_rows = x@Dim[1],
    proper_code = 0, # 0 = Igualdad
    is_binary = is_binary
  )

  # --- ¡LÓGICA CORREGIDA! ---
  # Tu .equal_sets_legacy SIEMPRE devuelve ngCMatrix, así que imitamos eso.
  M <- Matrix::sparseMatrix(
    i = res_list$i,
    p = res_list$p,
    index1 = FALSE,
    dims = c(y@Dim[2], x@Dim[2])
    # Siempre sin 'x' para crear ngCMatrix
  )
  return(as(Matrix::t(M), "nMatrix"))
}


.subset_legacy <- function(x, y = NULL, proper = FALSE) {
  if (!inherits(x, "CsparseMatrix")) {
    x <- convert_to_sparse(x)
  }

  if (is.null(y)) {
    y <- x
  } else {
    if (!inherits(y, "CsparseMatrix")) {
      y <- convert_to_sparse(y)
    }
  }

  # stopifnot("x" %in% methods::slotNames(x))
  # stopifnot("x" %in% methods::slotNames(y))

  p <- as.integer(rep(0, x@Dim[2] + 1))

  i <- is_subset_C(
    x@p, x@i, x@Dim, x@x,
    y@p, y@i, y@Dim, y@x,
    as.logical(proper), p
  )

  M <- build_sparse_matrix(
    i = i, p = p,
    dims = c(y@Dim[2], x@Dim[2])
  )

  M <- Matrix::t(M)

  empty <- Matrix::which(Matrix::colSums(x) == 0)

  if (length(empty) > 0) {
    M[empty, ] <- Matrix::Matrix(TRUE, ncol = y@Dim[2], nrow = length(empty))
  }

  return(M)
}

.equal_sets_legacy <- function(x, y = NULL, proper = FALSE) {
  if (!inherits(x, "CsparseMatrix")) {
    x <- convert_to_sparse(x)
  }

  if (is.null(y)) {
    y <- x
  } else {
    if (!inherits(y, "CsparseMatrix")) {
      y <- convert_to_sparse(y)
    }
  }

  # stopifnot("x" %in% methods::slotNames(x))
  # stopifnot("x" %in% methods::slotNames(y))

  p <- as.integer(rep(0, x@Dim[2] + 1))
  i <- is_equal_set_C(
    x@p, x@i, x@Dim, x@x,
    y@p, y@i, y@Dim, y@x,
    as.logical(proper), p
  )

  Matrix::t(methods::new("ngCMatrix",
    p = p, i = i,
    Dim = c(y@Dim[2], x@Dim[2])
  ))
}
