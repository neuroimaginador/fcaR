.simplify_lhs_rhs <- function(LHS, RHS) {


  LHS_subsets <- t(.is_subset_sparse(LHS))

  intersections <- self_intersection_sparse(LHS, RHS)

  new_LHS <- Matrix(0,
                    nrow = nrow(LHS),
                    ncol = 1,
                    sparse = TRUE)

  new_RHS <- Matrix(0,
                    nrow = nrow(LHS),
                    ncol = 1,
                    sparse = TRUE)

  # This gives the LHS that are subsets of other LHS
  condition1 <- colSums(LHS_subsets) > 1

  # This gives those LHS which are disjoint to their RHS
  condition2 <- intersections == 0

  are_subset <- which(condition1 & condition2)

  marked_as_single <- rep(TRUE, ncol(LHS))

  if (length(are_subset) > 0) {

    for (subs in seq_along(are_subset)) {

      this_row <- are_subset[subs]

      my_idx <- which_at_col_C(LHS_subsets@i,
                               LHS_subsets@p,
                               this_row)
      my_idx <- setdiff(my_idx, this_row)
      marked_as_single[my_idx] <- FALSE

      B <- as.matrix(RHS[, this_row])

      C <- as.matrix(LHS[, my_idx])
      D <- as.matrix(RHS[, my_idx])

      C_B <- sparse_set_difference(C, B)

      D_B <- sparse_set_difference(D, B)

      my_composition <- .compose_lhs_rhs_equal(LHS = Matrix(C_B, sparse = TRUE),
                                               RHS = Matrix(D_B, sparse = TRUE))

      new_LHS <- add_cols(new_LHS, my_composition$lhs)
      new_RHS <- add_cols(new_RHS, my_composition$rhs)

    }

  }

  singles <- which(marked_as_single)

  if (length(singles) > 0) {

    new_LHS <- cbind(new_LHS, LHS[, singles])
    new_RHS <- cbind(new_RHS, RHS[, singles])

  }

  return(list(lhs = new_LHS[, -1], rhs = new_RHS[, -1]))

}
