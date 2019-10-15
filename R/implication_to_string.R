.implication_to_string <- function(lhs, rhs, attributes) {

  A <- .set_to_string(lhs, attributes)
  B <- .set_to_string(rhs, attributes)

  paste0(A, " -> ", B)

}
