.compute_closure_binary <- function(S, LHS, RHS) {

  if (is.null(LHS) || (ncol(LHS) == 0)) return(S)

  # Which are the rules applicable to the set S?
  S_subsets <- .is_subset_binary(LHS, S)

  # idx_subsets <- which(S_subsets)
  idx_subsets <- S_subsets@i + 1

  do_not_use <- rep(FALSE, ncol(LHS))

  # While there are applicable rules, apply!!
  while (length(idx_subsets) > 0) {

    if (length(idx_subsets) == 1) {

      A <- Matrix(RHS[, idx_subsets], sparse = TRUE)

    } else {

      A <- RHS[, idx_subsets]

    }

    S <- .flatten_union_binary(add_col(A, S))

    do_not_use[idx_subsets] <- TRUE

    if (is.null(LHS) || (ncol(LHS) == 0)) {

      return(S)

    }

    S_subsets <- .is_subset_binary(LHS, S)

    idx_subsets <- S_subsets@i + 1
    idx_subsets <- setdiff(idx_subsets, which(do_not_use))

  }

  return(S)

}

.flatten_union_binary <- function(M) {

  sparseMatrix(i = unique(M@i) + 1,
               p = c(0, length(unique(M@i))),
               dims = c(nrow(M), 1))

}
