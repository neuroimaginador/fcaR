.clean <- function(LHS, RHS) {

  idx <- which(Matrix::colSums(RHS) == 0)

  if (length(idx) > 0) {

    LHS <- LHS[, -idx, drop = FALSE]
    RHS <- RHS[, -idx, drop = FALSE]

  }

  return(list(lhs = LHS, rhs = RHS))

}
