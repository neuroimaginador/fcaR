.clean <- function(LHS, RHS) {

  idx <- which(colSums(RHS) == 0)

  if (length(idx) > 0) {

    LHS <- LHS[, -idx]
    RHS <- RHS[, -idx]

  }

  return(list(lhs = LHS, rhs = RHS))

}
