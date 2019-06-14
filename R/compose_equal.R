.compose_lhs_rhs_equal <- function(LHS, RHS) {

  # logic_name <- tolower(fuzzy_logic()$name)

  equal_LHS <- .is_equal_set_sparse(LHS)

  replicas <- which(colSums(equal_LHS) > 1)

  marked_to_remove <- rep(FALSE, ncol(LHS))

  if (length(replicas) > 0) {

    for (rep_id in seq_along(replicas)) {

      if (marked_to_remove[replicas[rep_id]]) next

      ids_to_merge <- which_at_col_C(equal_LHS@i,
                                     equal_LHS@p,
                                     replicas[rep_id])

      B <- .flatten_union(RHS[, ids_to_merge])

      RHS[, ids_to_merge[1]] <- B

      marked_to_remove[ids_to_merge[-1]] <- TRUE

    }

  }

  remove_id <- which(marked_to_remove)

  if (length(remove_id) > 0) {

    LHS <- LHS[, -remove_id]
    RHS <- RHS[, -remove_id]

  }

  return(list(lhs = Matrix(LHS, sparse = TRUE),
              rhs = Matrix(RHS, sparse = TRUE)))

}
