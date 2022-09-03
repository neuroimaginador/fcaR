.composition <- function(LHS, RHS, attributes) {

  # logic_name <- tolower(fuzzy_logic()$name)
  LHS <- convert_to_sparse(LHS)
  RHS <- convert_to_sparse(RHS)

  equal_LHS <- .equal_sets(LHS)

  replicas <- Matrix::which(Matrix::colSums(equal_LHS) > 1)

  marked_to_remove <- rep(FALSE, ncol(LHS))

  if (length(replicas) > 0) {

    for (rep_id in seq_along(replicas)) {

      if (marked_to_remove[replicas[rep_id]]) next

      ids_to_merge <- which_at_col(equal_LHS@i,
                                     equal_LHS@p,
                                     replicas[rep_id])

      B <- .multiunion(RHS[, ids_to_merge])

      RHS[, ids_to_merge[1]] <- B

      marked_to_remove[ids_to_merge[-1]] <- TRUE

    }

  }

  remove_id <- which(marked_to_remove)

  if (length(remove_id) > 0) {

    LHS <- LHS[, -remove_id]
    RHS <- RHS[, -remove_id]

  }

  return(list(lhs = Matrix::Matrix(LHS, sparse = TRUE),
              rhs = Matrix::Matrix(RHS, sparse = TRUE)))

}
