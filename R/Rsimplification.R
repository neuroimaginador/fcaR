Rsimplification <- function(LHS, RHS, attributes, trace = FALSE) {

  LRHS_subsets <- zeroSpM(ncol.SpM(LHS),
                          ncol.SpM(LHS))
  intersections <- self_intersectSpM(lhs_bg, rhs_bg)

  id_inter <- Matrix::which(intersections == 0)

  # This gives the union of LHS and RHS

  M <- subsetSpM(extract_columns(LHS, id_inter), unionSpM(LHS, RHS))

  substitute_columns(LRHS_subsets, id_inter, M)

  # This gives the LRHS that are subsets of other LHS
  col_values <- colSums(LRHS_subsets)
  condition1 <- col_values > 1

  # This gives those LHS which are disjoint to their RHS
  condition2 <- intersections == 0

  are_subset <- which(condition1 & condition2)

  black_list <- rep(FALSE, ncol(LHS))

  count <- 0

  while (length(are_subset) > 0) {

    count <- count + 1

    id1 <- which.max(col_values[are_subset])
    this_row <- are_subset[id1]

    my_idx <- (LRHS_subsets %>% extract_columns(this_row))$pi

    # this_row <- id_inter[this_row]
    my_idx <- setdiff(my_idx, this_row)

    if (trace) {

      # original_rule <- ImplicationSet$new(name = "original",
      #                                     attributes = attributes,
      #                                     lhs = Matrix::Matrix(LHS[, this_row], sparse = TRUE),
      #                                     rhs = Matrix::Matrix(RHS[, this_row], sparse = TRUE))
      #
      # original_set <- ImplicationSet$new(name = "set",
      #                                    attributes = attributes,
      #                                    lhs = Matrix::Matrix(LHS[, my_idx], sparse = TRUE),
      #                                    rhs = Matrix::Matrix(RHS[, my_idx], sparse = TRUE))

    }

    # this_row is subset of all my_idx
    # So, we must do C -> D-B in every my_idx rule.

    C <- LHS %>% extract_columns(my_idx)
    D <- RHS %>% extract_columns(my_idx)

    B <- extract_columns(RHS, this_row)

    newRHS <- .difference2(D, B)

    if (trace) {

      # transformed_set <- ImplicationSet$new(name = "set",
      #                                       attributes = attributes,
      #                                       lhs = Matrix::Matrix(C, sparse = TRUE),
      #                                       rhs = Matrix::Matrix(newRHS, sparse = TRUE))
      #
      # message("Iteration", count, "\n")
      # message("=================\n")
      #
      # count <- count + 1
      #
      # message("** A -> B\n")
      # print(original_rule)
      #
      # message("** C -> D\n")
      # print(original_set)
      #
      # message("** C-B -> D-B\n")
      # print(transformed_set)

    }

    RHS %>% substitute_columns(my_idx, newRHS)

    if (trace) {
#
#       final_set <- ImplicationSet$new(name = "set",
#                                       attributes = attributes,
#                                       lhs = Matrix::Matrix(LHS, sparse = TRUE),
#                                       rhs = Matrix::Matrix(RHS, sparse = TRUE))
#
#       message("** Resulting set\n")
#       print(final_set)

    }
    intersections[my_idx] <- self_intersectSpM(C, newRHS)
    id_inter <- which(intersections == 0)

    M <- subsetSpM(LHS %>% extract_columns(id_inter),
                   unionSpM(C, newRHS))
    LRHS_subsets %>% substitute_columns(id_inter, M)
    col_values <- colSums(LRHS_subsets)
    condition1 <- col_values > 1

    condition2 <- (intersections == 0) & (colSums(RHS) > 0)

    black_list[this_row] <- TRUE
    are_subset <- which(condition1 & condition2 & (!black_list))

  }

  # Cleaning phase
  idx_to_remove <- which(colSums(RHS) == 0)

  if (length(idx_to_remove) > 0) {

    LHS <- LHS %>% remove_columns(idx_to_remove)
    RHS <- RHS %>% remove_columns(idx_to_remove)

  }

  return(list(lhs = LHS, rhs = RHS))

}

