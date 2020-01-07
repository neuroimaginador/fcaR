.simplification <- function(LHS, RHS, attributes, trace = FALSE) {

  LHS_subsets <- t(.subset(LHS))

  intersections <- .self_intersection(LHS, RHS)

  # This gives the LHS that are subsets of other LHS
  condition1 <- colSums(LHS_subsets) > 1

  # This gives those LHS which are disjoint to their RHS
  condition2 <- intersections == 0

  are_subset <- which(condition1 & condition2)

  black_list <- rep(FALSE, ncol(LHS))

  count <- 1

  while (length(are_subset) > 0) {

    this_row <- are_subset[1]

    my_idx <- which_at_col(LHS_subsets@i,
                           LHS_subsets@p,
                           this_row)
    my_idx <- setdiff(my_idx, this_row)

    if (trace) {

      original_rule <- ImplicationSet$new(name = "original",
                                           attributes = attributes,
                                           lhs = Matrix(LHS[, this_row], sparse = TRUE),
                                           rhs = Matrix(RHS[, this_row], sparse = TRUE))

      original_set <- ImplicationSet$new(name = "set",
                                          attributes = attributes,
                                          lhs = Matrix(LHS[, my_idx], sparse = TRUE),
                                          rhs = Matrix(RHS[, my_idx], sparse = TRUE))

    }

    # this_row is subset of all my_idx
    # So, we must do C-B -> D-B in every my_idx rule.
    newLHS <- .difference(LHS[, my_idx], RHS[, this_row])
    newRHS <- .difference(RHS[, my_idx], RHS[, this_row])

    if (trace) {

      transformed_set <- ImplicationSet$new(name = "set",
                                             attributes = attributes,
                                             lhs = Matrix(newLHS, sparse = TRUE),
                                             rhs = Matrix(newRHS, sparse = TRUE))

      message("Iteration", count, "\n")
      message("=================\n")

      count <- count + 1

      message("** A -> B\n")
      # print(original_rule)

      message("** C -> D\n")
      # print(original_set)

      message("** C-B -> D-B\n")
      # print(transformed_set)

    }

    LHS[, my_idx] <- newLHS
    RHS[, my_idx] <- newRHS

    if (trace) {

      final_set <- ImplicationSet$new(name = "set",
                                       attributes = attributes,
                                       lhs = Matrix(LHS, sparse = TRUE),
                                       rhs = Matrix(RHS, sparse = TRUE))

      message("** Resulting set\n")
      # print(final_set)

    }

    LHS_subsets <- t(.subset(LHS))

    condition1 <- colSums(LHS_subsets) > 1

    intersections <- .self_intersection(LHS, RHS)
    condition2 <- (intersections == 0) & (colSums(RHS) > 0)

    black_list[this_row] <- TRUE
    are_subset <- which(condition1 & condition2 & (!black_list))

  }

  # Cleaning phase
  idx_to_remove <- which(colSums(RHS) == 0)

  if (length(idx_to_remove) > 0) {

    LHS <- LHS[, -idx_to_remove]
    RHS <- RHS[, -idx_to_remove]

  }

  return(list(lhs = LHS, rhs = RHS))

}
