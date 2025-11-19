#' @title Calculate Concept Stability
#' @description Calculates the intensional stability of each concept.
#' @param extents A \code{SparseSet} or \code{CsparseMatrix} of extents.
#' @return A numeric vector.
#' @useDynLib fcaR, .registration = TRUE
#' @importFrom methods as
#' @importFrom Rcpp sourceCpp
#' @importClassesFrom Matrix CsparseMatrix
calculate_stability <- function(extents) {
  mat <- tryCatch(as(extents, "CsparseMatrix"), error = function(e) {
    stop("Input 'extents' must be convertible to a sparse matrix.")
  })
  return(calculate_stability_sparse_rcpp(mat))
}

#' @title Calculate Fuzzy Density
#' @description Calculates the density of each concept in the original matrix I.
#' @param extents A \code{SparseSet} or \code{CsparseMatrix} of extents.
#' @param intents A \code{SparseSet} or \code{CsparseMatrix} of intents.
#' @param I The original numeric matrix (FormalContext$I).
#' @return A numeric vector.
calculate_density <- function(extents, intents, I) {
  ext_mat <- as(extents, "CsparseMatrix")
  int_mat <- as(intents, "CsparseMatrix")

  if (is.null(I)) {
    # If I is missing (e.g. loaded from binary .cxt), assume binary density is 1
    return(rep(1.0, ncol(ext_mat)))
  }

  return(calculate_density_rcpp(ext_mat, int_mat, as.matrix(I)))
}

#' @title Calculate Concept Separation
#' @description Computes the separation of each concept.
#' Separation is defined as the number of objects in a concept's extent that are NOT covered
#' by any of its *immediate* subconcepts (children).
#'
#' @param lattice A \code{ConceptLattice} object.
#' @return A numeric vector of separation values.
#' @importFrom Matrix rowSums
#' @importFrom methods as
#' @export
calculate_separation <- function(lattice) {
  # 1. Obtener matriz de Extents (Objetos x Conceptos)
  # Usamos la matriz interna del SparseSet
  extents_mat <- as(lattice$extents, "CsparseMatrix")
  n_concepts <- ncol(extents_mat)

  # 2. Obtener la Relación de Cobertura (Hijos Directos)
  # Accedemos a la matriz de subconceptos (orden parcial)
  # El usuario indica que .reduce_transitivity convierte esto en covering
  # Asumimos que 'subconcept_matrix' está en el entorno privado del objeto,
  # pero desde fuera (esta función) no podemos acceder a private$.
  # Por tanto, esta lógica debe vivir dentro de la clase o usar un getter público.

  # SOLUCIÓN: Esta función auxiliar espera recibir ya la matriz de cobertura (adjacency).
  # Pero para mantener la firma simple, vamos a asumir que el usuario pasa
  # la matriz de adyacencia correcta como segundo argumento o que extraemos
  # la lógica de reducción aquí si tenemos acceso.

  stop("This function is intended to be called internally by ConceptLattice$separation()")
}

# --- VERSIÓN INTERNA (Helper) ---
# Esta función hace el trabajo sucio una vez tenemos la matriz de cobertura
calculate_separation_internal <- function(extents_mat, cover_mat) {
  n_concepts <- ncol(extents_mat)
  sep <- numeric(n_concepts)

  # cover_mat: Matriz dispersa donde M[i, j] = 1 si i es subconcepto de j.
  # Queremos los hijos de j: son los i tales que M[i, j] = 1 (y i != j).
  # (Asumiendo formato estándar: filas=sub, cols=super, o viceversa.
  #  En fcaR, subconcept_matrix suele ser S[i, j] = 1 si C_i <= C_j).
  #  Entonces, los hijos de C_j son las filas i con 1s en la columna j.

  # Optimizamos usando punteros de matriz dispersa (column-oriented)
  cover_mat <- as(cover_mat, "CsparseMatrix")

  # Iteramos sobre cada concepto (padre)
  for (j in 1:n_concepts) {
    # Índices de los hijos directos (filas con 1 en la columna j, excluyendo j si es reflexiva)
    # En una matriz dgCMatrix, los índices de fila para la columna j están en:
    # i_slots[ p[j] : p[j+1]-1 ]

    # R base access (lento pero seguro):
    # children_idx <- which(cover_mat[, j] != 0)

    # Acceso directo (Rápido):
    p_start <- cover_mat@p[j] + 1
    p_end <- cover_mat@p[j + 1]

    if (p_start > p_end) {
      # Sin hijos (Átomo) -> Separación = Tamaño del extent
      # (Aunque siempre debería haber el Bottom, salvo si es el Bottom mismo)
      sep[j] <- length(extents_mat@i[(extents_mat@p[j] + 1):extents_mat@p[j + 1]])
      next
    }

    children_idx <- cover_mat@i[p_start:p_end] + 1 # +1 por índice 0-based de C++
    children_idx <- children_idx[children_idx != j] # Quitar reflexividad si existe

    current_extent_size <- length(extents_mat@i[(extents_mat@p[j] + 1):extents_mat@p[j + 1]])

    if (length(children_idx) == 0) {
      sep[j] <- current_extent_size
    } else {
      # Calcular unión de extents de los hijos
      # Submatriz con las columnas de los hijos
      child_exts <- extents_mat[, children_idx, drop = FALSE]

      # Tamaño de la unión = número de filas con al menos un 1
      # rowSums es rápido en matrices dispersas
      union_size <- sum(Matrix::rowSums(child_exts) > 0)

      sep[j] <- current_extent_size - union_size
    }
  }

  return(sep)
}
