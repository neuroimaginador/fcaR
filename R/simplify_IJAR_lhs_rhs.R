# A(C-B) -> D (only for C not disjoint to B)
.simplify_IJAR_lhs_rhs <- function(LHS, RHS, attributes) {

  intersections <- .intersects_sparse(RHS, LHS)

  logic_name <- tolower(fuzzy_logic()$name)

  new_LHS <- LHS

  new_RHS <- RHS

  for (i in seq(ncol(LHS))) {

    with_intersection <- which_at_col_C(intersections@i,
                                        intersections@p,
                                        i)

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


      new_LHS <- add_cols(new_LHS, AC_B)
      new_RHS <- add_cols(new_RHS, my_RHS)

    }

  }

  return(list(lhs = new_LHS[, -1], rhs = new_RHS[, -1]))

}
