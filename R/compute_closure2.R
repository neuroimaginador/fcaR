.compute_closure2 <- function(S, LHS, RHS, reduce = FALSE, verbose = FALSE) {

  if (is.null(LHS) || (ncol(LHS) == 0)) return(S)

  # Which are the rules applicable to the set S?
  S_subsets <- .is_subset_sparse(LHS, S)

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

    S <- .flatten_union(add_col(A, S))

    if (verbose) {

      cat("Reducing", length(idx_subsets), " rules\n")

    }

    do_not_use[idx_subsets] <- TRUE


    if (is.null(LHS) || (ncol(LHS) == 0)) {

      if (!reduce) {

        return(S)

      } else {

        return(list(closure = S,
                    implications = list(lhs = LHS,
                                        rhs = RHS)))
      }

    }

    if (reduce) {

      if (verbose) {

        cat("Simplification stage\n")

      }

      C <- LHS
      D <- RHS

      CD <- apply_F_rowwise_xy(x = as.matrix(C),
                               y = as.matrix(D),
                               type = paste0("zadeh_S"))

      B <- S

      intersections <- .intersects_sparse(x = S,
                                          y = Matrix(CD,
                                                     sparse = TRUE))
      idx_not_empty <- which(colSums(intersections) > 0)

      C_B <- sparse_set_difference(C[, idx_not_empty], B)

      # C_B <- apply_F_rowwise_xy(x = C[, idx_not_empty],
      #                           y = B,
      #                           type = "set_diff")

      D_B <- sparse_set_difference(D[, idx_not_empty], B)

      # D_B <- apply_F_rowwise_xy(x = D[, idx_not_empty],
      #                           y = B,
      #                           type = "set_diff")

      idx_zeros <- which(colSums(D_B) == 0)

      if (verbose) {

        cat("Reducing", length(idx_zeros), " rules\n")

      }

      LHS <- cbind(Matrix(C_B[, -idx_zeros], sparse = TRUE),
                   Matrix(C[, -idx_not_empty], sparse = TRUE))
      RHS <- cbind(Matrix(D_B[, -idx_zeros], sparse = TRUE),
                   Matrix(D[, -idx_not_empty], sparse = TRUE))

    }

    S_subsets <- .is_subset_sparse(LHS, S)

    idx_subsets <- S_subsets@i + 1
    idx_subsets <- setdiff(idx_subsets, which(do_not_use))

  }

  if (reduce) return(list(closure = S,
                          implications = list(lhs = LHS,
                                              rhs = RHS)))
  return(S)

}

.flatten_union <- function(M) {

  v <- flatten_sparse_C(M@p, M@i, M@x, M@Dim)

  return(Matrix(v, ncol = 1, sparse = TRUE))
}
