.generalization <- function(LHS, RHS, attributes) {

  # A -> B and C -> D,
  # with A subset of C (axiom C -> A)
  # and D subset of B (axiom B -> D)
  #  => remove C -> D

  RHS_subsets <- subsetSpM(RHS)
  LHS_subsets <- tSpM(subsetSpM(LHS))

  ALL_subsets <- self_intersectSpM(LHS_subsets, RHS_subsets) # Was &

  # Find A subset of C
  condition1 <- colSums(ALL_subsets) > 1

  subsets <- which(condition1)

  marked_as_single <- rep(TRUE, ncol(LHS))

  if (length(subsets) > 0) {

    for (k in seq_along(subsets)) {

      # Index for A -> B
      this_row <- subsets[k]

      if (!marked_as_single[this_row]) next

      # Select C -> D with A subset of C and B superset of D
      idx_subset <- (ALL_subsets %>%
        extract_columns(this_row))$pi

      idx_subset <- setdiff(idx_subset, this_row)

      marked_as_single[idx_subset] <- FALSE

    }

  }

  # Add singles
  singles <- which(marked_as_single)

  LHS <- LHS %>% extract_columns(singles)
  RHS <- RHS %>% extract_columns(singles)

  return(list(lhs = LHS,
              rhs = RHS))

}
