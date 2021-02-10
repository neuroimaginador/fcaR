.simplification_bg <- function(lhs_bg, rhs_bg, lhs, rhs) {

  # lhs_bg <- imps_bg$get_LHS_matrix()
  # rhs_bg <- imps_bg$get_RHS_matrix()
  #
  # lhs <- imps$get_LHS_matrix()
  # rhs <- imps$get_RHS_matrix()

  intersections <- .self_intersection(lhs_bg, rhs_bg)

  id_inter <- Matrix::which(intersections == 0)

  if (length(id_inter) > 0) {

    lhs_bg <- Matrix::Matrix(lhs_bg[, id_inter], sparse = TRUE)
    rhs_bg <- Matrix::Matrix(rhs_bg[, id_inter], sparse = TRUE)

    LHS_subsets <- Matrix::t(.subset(lhs_bg, lhs))
    # This gives the lhs_bg that are subsets of LHS
    col_values <- Matrix::colSums(LHS_subsets)
    are_subset <- which(col_values > 0)
    black_list <- rep(FALSE, ncol(lhs_bg))

    count <- 0

    while (length(are_subset) > 0) {

      count <- count + 1

      id1 <- which.max(col_values[are_subset])
      this_row <- are_subset[id1]

      my_idx <- which_at_col(LHS_subsets@i,
                             LHS_subsets@p,
                             this_row)

      # this_row is subset of all my_idx
      # So, we must do C-B -> D-B in every my_idx rule.

      if (length(my_idx) > 1) {

        C <- lhs[, my_idx]
        D <- rhs[, my_idx]

      } else {

        C <- Matrix::Matrix(lhs[, my_idx], sparse = TRUE)
        D <- Matrix::Matrix(rhs[, my_idx], sparse = TRUE)

      }
      B <- Matrix::Matrix(rhs_bg[, this_row], sparse = TRUE)
      newLHS <- set_difference_single(C@i, C@p, C@x,
                                      B@i, B@p, B@x,
                                      nrow(C))
      newRHS <- set_difference_single(D@i, D@p, D@x,
                                      B@i, B@p, B@x,
                                      nrow(D))

      lhs[, my_idx] <- newLHS
      rhs[, my_idx] <- newRHS


      LHS_subsets[my_idx, ] <- Matrix::t(.subset(lhs_bg, newLHS))
      col_values <- Matrix::colSums(LHS_subsets)
      condition1 <- which(col_values > 1)

      black_list[this_row] <- TRUE
      are_subset <- Matrix::which(condition1 & (!black_list))

    }

    # Cleaning phase
    idx_to_remove <- Matrix::which(Matrix::colSums(rhs) == 0)

    if (length(idx_to_remove) > 0) {

      lhs <- lhs[, -idx_to_remove]
      rhs <- rhs[, -idx_to_remove]

    }

  }

  return(list(lhs = lhs, rhs = rhs))

  # return(ImplicationSet$new(attributes = imps$get_attributes(),
  #                           lhs = lhs, rhs = rhs))

}
