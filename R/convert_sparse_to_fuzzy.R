.convert_sparse_to_fuzzy <- function(LHS, RHS, attributes) {

  idx_lhs <- which(LHS > 0)
  idx_rhs <- which(RHS > 0)

  imp <- NULL

  if (length(idx_rhs) > 0) {

    B <- gset(support = attributes[idx_rhs], memberships = RHS[idx_rhs])

    if (length(idx_lhs) > 0) {

      A <- gset(support = attributes[idx_lhs], memberships = LHS[idx_lhs])

    } else {

      A <- gset()

    }

    imp <- implication$new(lhs = A, rhs = B)

  }

  return(imp)

}
