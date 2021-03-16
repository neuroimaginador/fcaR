.simplificationSpM <- function(LHS, RHS, attributes) {

  LHS_subsets <- zeroSpM(ncol.SpM(LHS),
                         ncol.SpM(LHS))

  intersections <- self_intersectSpM(LHS, RHS)

  id_inter <- which(intersections == 0)

  M <- subsetSpM(extract_columns(LHS, id_inter), LHS)

  substitute_columns(LHS_subsets, id_inter, M)
  # LHS_subsets[, id_inter] <- Matrix::t(.subset(LHS[, id_inter], LHS))

  # This gives the LHS that are subsets of other LHS
  col_values <- colSums(LHS_subsets)
  condition1 <- col_values > 1

  # This gives those LHS which are disjoint to their RHS
  condition2 <- intersections == 0

  are_subset <- which(condition1 & condition2)

  black_list <- rep(FALSE, ncol.SpM(LHS))

  count <- 0

  while (length(are_subset) > 0) {

    count <- count + 1

    id1 <- which.max(col_values[are_subset])
    this_row <- are_subset[id1]

    my_idx <- (LHS_subsets %>%
      extract_columns(this_row))$pi#[[1]]

    # this_row <- id_inter[this_row]
    my_idx <- setdiff(my_idx, this_row)

    # this_row is subset of all my_idx
    # So, we must do C-B -> D-B in every my_idx rule.
    C <- LHS %>% extract_columns(my_idx)
    D <- RHS %>% extract_columns(my_idx)

    B <- RHS %>% extract_columns(this_row)

    # newLHS <- SpM$new(
    #   set_difference_single(
    #     unlist(C$i()) - 1, C$p(), unlist(C$x()),
    #     unlist(B$i()) - 1, B$p(), unlist(B$x()),
    #     C$nrow()))
    #
    # newRHS <- SpM$new(
    #   set_difference_single(
    #     unlist(D$i()) - 1, D$p(), unlist(D$x()),
    #     unlist(B$i()) - 1, B$p(), unlist(B$x()),
    #     D$nrow()))

    newLHS <- .difference2(C, B)
    # new_spm(
    #   set_difference_single(
    #     C$pi - 1, C$pp, C$px,
    #     B$pi - 1, B$pp, B$px,
    #     nrow.SpM(C)))

    newRHS <- .difference2(D, B)
      # new_spm(
      # set_difference_single(
      #   D$pi - 1, D$pp, D$px,
      #   B$pi - 1, B$pp, B$px,
      #   nrow.SpM(D)))

    LHS %>% substitute_columns(my_idx, newLHS)
    RHS %>% substitute_columns(my_idx, newRHS)

    intersections[my_idx] <- self_intersectSpM(newLHS, newRHS)
    id_inter <- which(intersections == 0)

    M <- subsetSpM(LHS %>% extract_columns(id_inter), LHS)
    LHS_subsets %>% substitute_columns(id_inter, M)
    col_values <- LHS_subsets %>% colSums()
    condition1 <- col_values > 1

    condition2 <- (intersections == 0) & (RHS %>% colSums() > 0)

    black_list[this_row] <- TRUE
    are_subset <- which(condition1 & condition2 & (!black_list))

  }

  # Cleaning phase
  idx_to_remove <- which(colSums(RHS) == 0)

  if (length(idx_to_remove) > 0) {

    LHS %>% remove_columns(idx_to_remove)
    RHS %>% remove_columns(idx_to_remove)

  }

  return(list(lhs = LHS, rhs = RHS))

}
