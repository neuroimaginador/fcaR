#' @import Matrix
#' @export
build_set <- function(attrs, values, attributes) {

  v <- rep(0, length(attributes))
  names(v) <- attributes

  v[attrs] <- values

  Matrix(v, nrow = length(attributes), ncol = 1, sparse = TRUE)

}
