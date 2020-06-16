.implication_to_string <- function(lhs, rhs, attributes, dictionary) {

  A <- .set_to_string(lhs, attributes, dictionary)
  B <- .set_to_string(rhs, attributes, dictionary)

  paste0(A, " -> ", B)

}
