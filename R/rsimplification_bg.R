.Rsimplification_bg <- function(lhs_bg, rhs_bg, LHS, RHS) {

  LRHS_subsets <- zeroSpM(ncol = ncol.SpM(lhs_bg),
                          nrow = ncol.SpM(LHS))
  intersections <- self_intersectSpM(lhs_bg, rhs_bg)

  id_inter <- which(intersections == 0)

  # This gives the union of LHS and RHS

  M <- subsetSpM(extract_columns(lhs_bg, id_inter), unionSpM(LHS, RHS))

  substitute_columns(LRHS_subsets, id_inter, M)
  #
  #
  # LRHS_subsets[, id_inter] <- Matrix::t(.subset(lhs_bg[, id_inter], .union(LHS,RHS)))

  # This gives the LRHS that are subsets of other LHS
  col_values <- colSums(LRHS_subsets)
  condition1 <- col_values > 0

  # This gives those LHS which are disjoint to their RHS
  # condition2 <- intersections == 0
  are_subset <- which(condition1)

  black_list <- rep(FALSE, ncol.SpM(lhs_bg))

  count <- 0

  while (length(are_subset) > 0) {

    count <- count + 1

    id1 <- which.max(col_values[are_subset])
    this_row <- are_subset[id1]

    # print(this_row)

    my_idx <- (LRHS_subsets %>% extract_columns(this_row))$pi

    # this_row is subset of all my_idx
    # So, we must do C -> D-B in every my_idx rule.

    C <- LHS %>% extract_columns(my_idx)
    D <- RHS %>% extract_columns(my_idx)

    B <- extract_columns(rhs_bg, this_row)
    newRHS <- .difference2(D, B)

    RHS %>% substitute_columns(my_idx, newRHS)

    # TODO: In some places, subsetSpM needs to be transposed.
    M <- subsetSpM(lhs_bg %>% extract_columns(id_inter), unionSpM(C, newRHS))
    LRHS_subsets %>% substitute_columns(id_inter, M)
    col_values <- colSums(LRHS_subsets)
    condition1 <- col_values > 0

    black_list[this_row] <- TRUE
    are_subset <- which(condition1 & (!black_list))

  }

  # Cleaning phase
  idx_to_remove <- which(colSums(RHS) == 0)

  if (length(idx_to_remove) > 0) {

    LHS <- LHS %>% remove_columns(idx_to_remove)
    RHS <- RHS %>% remove_columns(idx_to_remove)

  }

  return(list(lhs = LHS, rhs = RHS))

}
