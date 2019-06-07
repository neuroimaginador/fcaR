.batch_reduce_lhs_rhs <- function(LHS, RHS, batch_size = 25000L) {

  n_implications <- ncol(LHS)
  # Introduce some random order into implications
  ordering <- sample(seq(n_implications),
                     size = n_implications)

  LHS <- LHS[, ordering]
  RHS <- RHS[, ordering]

  new_LHS <- Matrix(0,
                    nrow = nrow(LHS),
                    ncol = 1,
                    sparse = TRUE)

  new_RHS <- Matrix(0,
                    nrow = nrow(LHS),
                    ncol = 1,
                    sparse = TRUE)

  idx <- c(seq(1, n_implications, by = batch_size),
           n_implications + 1)

  for (i in seq_along(idx[-1])) {

    my_cols <- idx[i + 1] - idx[i]

    cat("Processing chunk", i, "out of", length(idx) - 1, "\n")

    old_LHS <- LHS[, idx[i]:(idx[i + 1] - 1)]
    old_RHS <- RHS[, idx[i]:(idx[i + 1] - 1)]

    L <- .compose_lhs_rhs_equal(old_LHS,
                                old_RHS)

    old_LHS <- L$lhs
    old_RHS <- L$rhs

    composition_cols <- ncol(old_LHS)
    cat("--> Composition: from", my_cols, "to",
        composition_cols, "\n")

    L <- .remove_redundancies_lhs_rhs_general(old_LHS,
                                              old_RHS)
    old_LHS <- L$lhs
    old_RHS <- L$rhs

    redu_cols <- ncol(old_LHS)
    cat("--> Redundancies removal: from", composition_cols, "to",
        redu_cols, "\n")


    new_LHS <- cbind(new_LHS, old_LHS)
    new_RHS <- cbind(new_RHS, old_RHS)

  }

  LHS <- new_LHS[, -1]
  RHS <- new_RHS[, -1]

  return(list(lhs = LHS, rhs = RHS))

}
