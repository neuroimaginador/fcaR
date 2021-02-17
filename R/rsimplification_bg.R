.Rsimplification_bg <- function(lhs_bg, rhs_bg, LHS, RHS) {

  LRHS_subsets <- Matrix::Matrix(FALSE, sparse = TRUE,
                                 ncol = ncol(lhs_bg),
                                 nrow = ncol(LHS))
  intersections <- .self_intersection(lhs_bg, rhs_bg)

  id_inter <- Matrix::which(intersections == 0)

  # This gives the union of LHS and RHS

  LRHS_subsets[, id_inter] <- Matrix::t(.subset(lhs_bg[, id_inter], .union(LHS,RHS)))

  # This gives the LRHS that are subsets of other LHS
  col_values <- Matrix::colSums(LRHS_subsets)
  condition1 <- col_values > 0

  # This gives those LHS which are disjoint to their RHS
  # condition2 <- intersections == 0

  are_subset <- which(condition1)

  black_list <- rep(FALSE, ncol(lhs_bg))

  count <- 0

  while (length(are_subset) > 0) {

    count <- count + 1

    id1 <- which.max(col_values[are_subset])
    this_row <- are_subset[id1]

    # print(this_row)

    my_idx <- which_at_col(LRHS_subsets@i,
                           LRHS_subsets@p,
                           this_row)

    # this_row <- id_inter[this_row]
    #my_idx <- setdiff(my_idx, this_row)


    # this_row is subset of all my_idx
    # So, we must do C -> D-B in every my_idx rule.

    if (length(my_idx) > 1) {

      C <- LHS[, my_idx]
      D <- RHS[, my_idx]

    } else {

      C <- Matrix::Matrix(LHS[, my_idx], sparse = TRUE)
      D <- Matrix::Matrix(RHS[, my_idx], sparse = TRUE)

    }
    B <- Matrix::Matrix(rhs_bg[, this_row], sparse = TRUE)
    newRHS <- set_difference_single(D@i, D@p, D@x,
                                    B@i, B@p, B@x,
                                    nrow(D))


    RHS[, my_idx] <- newRHS

    LRHS_subsets[my_idx, id_inter] <- Matrix::t(.subset(lhs_bg[, id_inter], .union(C, newRHS)))
    col_values <- Matrix::colSums(LRHS_subsets)
    condition1 <- col_values > 0

    # condition2 <- (intersections == 0) & (Matrix::colSums(RHS) > 0)

    black_list[this_row] <- TRUE
    are_subset <- Matrix::which(condition1 & (!black_list))

  }

  # Cleaning phase
  idx_to_remove <- Matrix::which(Matrix::colSums(RHS) == 0)

  if (length(idx_to_remove) > 0) {

    LHS <- LHS[, -idx_to_remove]
    RHS <- RHS[, -idx_to_remove]

  }

  return(list(lhs = LHS, rhs = RHS))

}
