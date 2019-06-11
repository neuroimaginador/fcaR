.implication_to_string <- function(lhs, rhs, attributes) {

  A <- .sparse_set_to_string(lhs, attributes)
  B <- .sparse_set_to_string(rhs, attributes)

  paste0(A, " -> ", B)

}
