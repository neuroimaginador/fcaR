#' @title Calculate Concept Stability
#' @description Calculates the intensional stability of each concept in the lattice directly from the sparse matrix representation.
#' Stability measures the probability that a concept is preserved when a random subset of objects is removed.
#'
#' @param extents A \code{SparseSet} object or a sparse matrix (\code{CsparseMatrix}) representing concept extents (columns are concepts).
#' @return A numeric vector with stability values in \eqn{[0, 1]}.
#' @useDynLib fcaR, .registration = TRUE
#' @importFrom methods as
#' @importFrom Rcpp sourceCpp
#' @importClassesFrom Matrix CsparseMatrix
calculate_stability <- function(extents) {
  # Asegurar que tenemos una matriz dispersa comprimida por columnas (CsparseMatrix)
  # Si es un SparseSet de fcaR, asumimos que se puede convertir
  mat <- tryCatch(as(extents, "CsparseMatrix"), error = function(e) {
    stop("Input 'extents' must be convertible to a sparse matrix (CsparseMatrix).")
  })

  return(calculate_stability_sparse_rcpp(mat))
}
