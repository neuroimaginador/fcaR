.generalization <- function(LHS, RHS, attributes) {

  # A -> B and C -> D,
  # with A subset of C (axiom C -> A)
  # and D subset of B (axiom B -> D)
  #  => remove C -> D

  RHS_subsets <- .subset(RHS)
  LHS_subsets <- Matrix::t(.subset(LHS))

  ALL_subsets <- LHS_subsets & RHS_subsets

  # Find A subset of C
  condition1 <- Matrix::colSums(ALL_subsets) > 1

  subsets <- Matrix::which(condition1)

  marked_as_single <- rep(TRUE, ncol(LHS))

  if (length(subsets) > 0) {

    for (k in seq_along(subsets)) {

      # Index for A -> B
      this_row <- subsets[k]

      if (!marked_as_single[this_row]) next

      # Select C -> D with A subset of C and B superset of D
      idx_subset <- which_at_col(ALL_subsets@i,
                                 ALL_subsets@p,
                                 this_row)

      idx_subset <- setdiff(idx_subset, this_row)

      marked_as_single[idx_subset] <- FALSE

    }

  }

  # Add singles
  singles <- which(marked_as_single)

  LHS <- LHS[, singles]
  RHS <- RHS[, singles]

  return(list(lhs = Matrix::Matrix(LHS, sparse = TRUE),
              rhs = Matrix::Matrix(RHS, sparse = TRUE)))

}
