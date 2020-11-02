#' Convert Named Vector to SparseSet
#'
#' @param A A named vector or matrix to build a new \code{SparseSet}.
#'
#' @return A \code{SparseSet} object.
#' @export
#'
#' @examples
#' A <- c(a = 0.1, b = 0.2, p = 0.3, q = 0)
#' as_SparseSet(A)
#'
as_SparseSet <- function(A) {

  attributes <- names(A)
  SparseSet$new(attributes = attributes,
                M = Matrix::Matrix(A, sparse = TRUE))

}

#' Convert SparseSet to vector
#'
#' @param v A \code{SparseSet} to convert to vector.
#'
#' @return A vector.
#' @export
#'
#' @examples
#' A <- c(a = 0.1, b = 0.2, p = 0.3, q = 0)
#' v <- as_SparseSet(A)
#' A2 <- as_vector(v)
#' all(A == A2)
#'
as_vector <- function(v) {

  A <- as.numeric(v$get_vector())
  names(A) <- v$get_attributes()

  return(A)
}
