# A(C-B) -> D (only for C not disjoint to B)
.simplify_IJAR_lhs_rhs <- function(LHS, RHS) {

  intersections <- .intersects_sparse(RHS, LHS)

  logic_name <- tolower(fuzzy_logic()$name)

  new_LHS <- LHS

  new_RHS <- RHS

  for (i in seq(ncol(LHS))) {

    with_intersection <- which_at_col(intersections, i)

    with_intersection <- setdiff(with_intersection, i)

    if (length(with_intersection) > 0) {

      A <- as.matrix(LHS[, i])
      B <- as.matrix(RHS[, i])

      C <- as.matrix(LHS[, with_intersection])
      # D <- as.matrix(RHS[, my_idx])

      C_B <- apply_F_rowwise_xy(x = C,
                                y = B,
                                type = "set_diff")

      AC_B <- apply_F_rowwise_xy(x = C_B,
                                 y = A,
                                 type = paste0(logic_name, "_S"))

      AC_B <- Matrix(AC_B, sparse = TRUE)
      my_RHS <- Matrix(RHS[, with_intersection], sparse = TRUE)


      new_LHS <- cbind(new_LHS, AC_B)
      new_RHS <- cbind(new_RHS, my_RHS)

    }

  }

  # # implication_list <- list()
  #
  # are_subset <- which(rowSums(private$lhs_subsets) > 1)
  #
  # marked_as_single <- rep(TRUE, ncol(LHS))
  #
  # if (length(are_subset) > 0) {
  #
  #   for (subs in seq_along(are_subset)) {
  #
  #     this_row <- are_subset[subs]
  #
  #     my_idx <- which(private$lhs_subsets[this_row, ])
  #     marked_as_single[my_idx] <- FALSE
  #
  #     my_idx <- setdiff(my_idx, this_row)
  #     A <- as.matrix(LHS[, this_row])
  #     B <- as.matrix(RHS[, this_row])
  #
  #     C <- as.matrix(LHS[, my_idx])
  #     # D <- as.matrix(RHS[, my_idx])
  #
  #     C_B <- apply_F_rowwise_xy(x = C,
  #                               y = B,
  #                               type = "set_diff")
  #
  #     AC_B <- apply_F_rowwise_xy(x = C_B,
  #                                y = A,
  #                                type = paste0(logic_name, "_S"))
  #
  #     new_LHS <- cbind(new_LHS, AC_B)
  #     new_RHS <- cbind(new_RHS, RHS[, my_idx])
  #
  #   }
  #
  # }
  #
  # singles <- which(marked_as_single)
  #
  # if (length(singles) > 0) {
  #
  #   new_LHS <- cbind(new_LHS, LHS[, singles])
  #   new_RHS <- cbind(new_RHS, RHS[, singles])
  #
  # }

  return(list(lhs = new_LHS[, -1], rhs = new_RHS[, -1]))

}
