#' Convert Named Vector to Set
#'
#' @param A A named vector or matrix to build a new \code{Set}.
#'
#' @return A \code{Set} object.
#' @export
#'
#' @examples
#' A <- c(a = 0.1, b = 0.2, p = 0.3, q = 0)
#' as_Set(A)
#'
as_Set <- function(A) {

  attributes <- names(A)
  Set$new(attributes = attributes,
          M = Matrix::Matrix(A, sparse = TRUE))

}

#' Convert Set to vector
#'
#' @param v A \code{Set} to convert to vector.
#'
#' @return A vector.
#' @export
#'
#' @examples
#' A <- c(a = 0.1, b = 0.2, p = 0.3, q = 0)
#' v <- as_Set(A)
#' A2 <- as_vector(v)
#' all(A == A2)
#'
as_vector <- function(v) {

  A <- as.numeric(v$get_vector())
  names(A) <- v$get_attributes()

  return(A)
}
