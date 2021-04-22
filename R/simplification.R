.simplification <- function(LHS, RHS, attributes, trace = FALSE) {

  LHS_subsets <- Matrix::Matrix(FALSE, sparse = TRUE,
                                ncol = ncol(LHS),
                                nrow = ncol(LHS))
  intersections <- .self_intersection(LHS, RHS)

  id_inter <- Matrix::which(intersections == 0)

  LHS_subsets[, id_inter] <- Matrix::t(.subset(LHS[, id_inter], LHS))

  # This gives the LHS that are subsets of other LHS
  col_values <- Matrix::colSums(LHS_subsets)
  condition1 <- col_values > 1

  # This gives those LHS which are disjoint to their RHS
  condition2 <- intersections == 0

  are_subset <- Matrix::which(condition1 & condition2)

  black_list <- rep(FALSE, ncol(LHS))

  count <- 0

  while (length(are_subset) > 0) {

    count <- count + 1

    id1 <- which.max(col_values[are_subset])
    this_row <- are_subset[id1]

    my_idx <- which_at_col(LHS_subsets@i,
                           LHS_subsets@p,
                           this_row)

    # this_row <- id_inter[this_row]
    my_idx <- setdiff(my_idx, this_row)

    # this_row is subset of all my_idx
    # So, we must do C-B -> D-B in every my_idx rule.

    if (length(my_idx) > 1) {

      C <- LHS[, my_idx]
      D <- RHS[, my_idx]

    } else {

      C <- Matrix::Matrix(LHS[, my_idx], sparse = TRUE)
      D <- Matrix::Matrix(RHS[, my_idx], sparse = TRUE)

    }
    B <- Matrix::Matrix(RHS[, this_row], sparse = TRUE)
    newLHS <- set_difference_single(C@i, C@p, C@x,
                                    B@i, B@p, B@x,
                                    nrow(C))
    newRHS <- set_difference_single(D@i, D@p, D@x,
                                    B@i, B@p, B@x,
                                    nrow(D))

    LHS[, my_idx] <- newLHS
    RHS[, my_idx] <- newRHS

    intersections[my_idx] <- .self_intersection(newLHS, newRHS)
    id_inter <- which(intersections == 0)

    LHS_subsets[my_idx, id_inter] <- Matrix::t(.subset(LHS[, id_inter], newLHS))
    col_values <- Matrix::colSums(LHS_subsets)
    condition1 <- col_values > 1

    condition2 <- (intersections == 0) & (Matrix::colSums(RHS) > 0)

    black_list[this_row] <- TRUE
    are_subset <- Matrix::which(condition1 & condition2 & (!black_list))

  }

  # Cleaning phase
  idx_to_remove <- Matrix::which(Matrix::colSums(RHS) == 0)

  if (length(idx_to_remove) > 0) {

    LHS <- LHS[, -idx_to_remove]
    RHS <- RHS[, -idx_to_remove]

  }

  return(list(lhs = LHS, rhs = RHS))

}
