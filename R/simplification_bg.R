.simplification_bg <- function(lhs_bg, rhs_bg, lhs, rhs) {

  intersections <- self_intersectSpM(lhs_bg, rhs_bg)

  id_inter <- which(intersections == 0)

  if (length(id_inter) > 0) {

    lhs_bg <- lhs_bg %>% extract_columns(id_inter)
    rhs_bg <- rhs_bg %>% extract_columns(id_inter)

    # TODO: transpose??
    LHS_subsets <- subsetSpM(lhs_bg, lhs)
    # This gives the lhs_bg that are subsets of LHS
    col_values <- colSums(LHS_subsets)
    are_subset <- which(col_values > 0)
    black_list <- rep(FALSE, ncol.SpM(lhs_bg))

    count <- 0

    while (length(are_subset) > 0) {

      count <- count + 1

      id1 <- which.max(col_values[are_subset])
      this_row <- are_subset[id1]

      my_idx <- (LHS_subsets %>% extract_columns(this_row))$pi

      # this_row is subset of all my_idx
      # So, we must do C-B -> D-B in every my_idx rule.

      C <- lhs %>% extract_columns(my_idx)
      D <- rhs %>% extract_columns(my_idx)

      B <- extract_columns(rhs_bg, this_row)
      newLHS <- .difference2(C, B)
      newRHS <- .difference2(D, B)

      lhs %>% substitute_columns(my_idx, newLHS)
      rhs %>% substitute_columns(my_idx, newRHS)

      foo <- LHS_subsets %>% tSpM()
      foo %>%
        substitute_columns(my_idx, tSpM(subsetSpM(lhs_bg, newLHS)))

      LHS_subsets <- tSpM(foo)
      col_values <- colSums(LHS_subsets)
      condition1 <- col_values > 0

      black_list[this_row] <- TRUE
      are_subset <- which(condition1 & (!black_list))

    }

    # browser()

    # Cleaning phase
    idx_to_remove <- which(colSums(rhs) == 0)

    if (length(idx_to_remove) > 0) {

      lhs <- lhs %>% remove_columns(idx_to_remove)
      rhs <- rhs %>% remove_columns(idx_to_remove)

    }

  }

  return(list(lhs = lhs, rhs = rhs))

}
