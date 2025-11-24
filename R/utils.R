#' @importFrom Matrix summary
sparse_to_list <- function(sparse_set_or_matrix) {
  # Si ya es lista, devolver tal cual
  if (is.list(sparse_set_or_matrix) && !inherits(sparse_set_or_matrix, "Matrix")) {
    return(sparse_set_or_matrix)
  }

  # Si es un objeto SparseSet de fcaR, extraemos la matriz interna
  # (Asumo que tiene un campo @matrix o similar, o usamos as(..., "nMatrix"))
  # Para ser genéricos, intentamos convertir a matriz dispersa primero.
  mat <- tryCatch(as(sparse_set_or_matrix, "nMatrix"), error = function(e) sparse_set_or_matrix)

  # Usar summary para obtener tripletes (i, j)
  # j es la columna (el concepto), i es la fila (el objeto/atributo)
  s <- Matrix::summary(mat)

  # Crear la lista vacía del tamaño correcto (número de columnas)
  n_concepts <- ncol(mat)
  res <- vector("list", n_concepts)

  # Llenar la lista usando split (muy rápido en R)
  # s$j son índices base-1 de columnas
  # s$i son índices base-1 de filas
  split_data <- split(s$i, s$j)

  # Asignar a las posiciones correctas (por si hay columnas vacías que split omite)
  res[as.integer(names(split_data))] <- split_data

  return(res)
}


#' Check if a suggested package is installed
#'
#' @param pkg Character. The name of the package.
#' @param purpose Character. A brief description of why it is needed.
#'
#' @noRd
check_needed_pkg <- function(pkg, purpose) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      glue::glue(
        "The package '{pkg}' is required for {purpose}.\n",
        "Please install it using: install.packages('{pkg}')"
      ),
      call. = FALSE
    )
  }
}
