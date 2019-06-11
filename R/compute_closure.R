.compute_closure <- function(S, LHS, RHS) {

  if (is.null(LHS) || (ncol(LHS) == 0)) return(S)

  # Which are the rules applicable to the set S?
  S_subsets <- .is_subset_sparse(LHS, S)

  idx_subsets <- which(S_subsets)

  # While there are applicable rules, apply!!
  while (length(idx_subsets) > 0) {

    S <- .flatten_union(cbind(RHS[, idx_subsets], S))

    LHS <- Matrix(LHS[, -idx_subsets], sparse = TRUE)
    RHS <- Matrix(RHS[, -idx_subsets], sparse = TRUE)

    # print(LHS)

    if (is.null(LHS) || (ncol(LHS) == 0)) return(S)

    S_subsets <- .is_subset_sparse(LHS, S)

    idx_subsets <- which(S_subsets)

  }

  return(S)

}

.flatten_union <- function(M) {

  v <- flatten_sparse_C(M@p, M@i, M@x, M@Dim)

  return(Matrix(v, ncol = 1, sparse = TRUE))
}
