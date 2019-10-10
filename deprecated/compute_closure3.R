.compute_closure3 <- function(S, tree, RHS,
                              reduce = FALSE,
                              verbose = FALSE) {

  if (is.null(RHS) || (ncol(RHS) == 0)) return(S)

  n_implications <- ncol(RHS)
  do_not_use <- rep(FALSE, ncol(RHS))
  idx_subsets <- which(is_subset_tree_XPtr(tree, S))

  # While there are applicable rules, apply!!
  while (length(idx_subsets) > 0) {

    if (length(idx_subsets) == 1) {

      A <- Matrix(RHS[, idx_subsets], sparse = TRUE)

    } else {

      A <- RHS[, idx_subsets]

    }

    S <- .flatten_union(add_col(A, S))

    do_not_use[idx_subsets] <- TRUE

    if (all(do_not_use)) {

      if (!reduce) {

        return(S)

      }

    }

      idx_subsets <- which(is_subset_tree_XPtr(tree, S))
      idx_subsets <- setdiff(idx_subsets, which(do_not_use))

  }

  return(S)

}

.flatten_union <- function(M) {

  v <- flatten_sparse_C(M@p, M@i, M@x, M@Dim)

  return(Matrix(v, ncol = 1, sparse = TRUE))
}
