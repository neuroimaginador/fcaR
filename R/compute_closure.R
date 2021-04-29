.compute_closure <- function(S, LHS, RHS, attributes,
                             reduce = FALSE, verbose = FALSE,
                             is_direct = FALSE) {

  if (is.null(LHS) || (ncol(LHS) == 0)) {

    return(list(closure = S,
                implications = list(lhs = LHS,
                                    rhs = RHS)))

  }

  # Which are the rules applicable to the set S?
  S_subsets <- .subset(LHS, S)

  idx_subsets <- S_subsets@i + 1

  do_not_use <- rep(FALSE, ncol(LHS))

  passes <- 0

  # While there are applicable rules, apply!!
  while (length(idx_subsets) > 0) {

    passes <- passes + 1
    if (verbose) cat("Pass #", passes, "\n")

    if (length(idx_subsets) == 1) {

      A <- Matrix::Matrix(RHS[, idx_subsets], sparse = TRUE)

    } else {

      A <- RHS[, idx_subsets]

    }

    S <- .multiunion(add_col(A, S))

    do_not_use[idx_subsets] <- TRUE

    if (reduce) {

      L <- .simplification_logic(S = S,
                                 LHS = LHS,
                                 RHS = RHS)

      LHS <- L$lhs
      RHS <- L$rhs

      for (rem in L$idx_removed) {

        do_not_use <- do_not_use[-rem]

      }

    }


    if (is.null(LHS) || (ncol(LHS) == 0)) {

      return(list(closure = S,
                  implications = list(lhs = LHS,
                                      rhs = RHS)))
    }

    if (!is_direct) {

      S_subsets <- .subset(LHS, S)

      idx_subsets <- S_subsets@i + 1
      idx_subsets <- setdiff(idx_subsets, which(do_not_use))

      if (verbose) {

        print(idx_subsets)
        print(Set$new(attributes = attributes,
                            M = S))
        cat("\n")

      }


    } else {

      idx_subsets <- c()

    }

  }

  if (reduce) {

    return(list(closure = S,
                implications = .simplification_logic(S,
                                                     LHS,
                                                     RHS)))

  } else {

    return(list(closure = S,
                implications = list(LHS,
                                    RHS)))

  }

}

.simplification_logic <- function(S, LHS, RHS) {

  # Equivalence II
  subsets <- .subset(RHS, S)
  idx_subsets <- subsets@i + 1

  idx_removed <- list()

  if (length(idx_subsets) > 0) {

    idx_removed[[1]] <- idx_subsets

    LHS <- Matrix::Matrix(LHS[, -idx_subsets], sparse = TRUE)
    RHS <- Matrix::Matrix(RHS[, -idx_subsets], sparse = TRUE)

  }

  if (ncol(LHS) == 0) {

    return(list(lhs = NULL, rhs = NULL))

  }

  # Equivalence III
  C <- LHS
  D <- RHS

  CD <- .union(LHS, RHS)

  intersections <- .intersection(x = S, y = CD)
  idx_not_empty <- Matrix::which(Matrix::colSums(intersections) > 0)

  if (length(idx_not_empty) > 0) {

    if (length(idx_not_empty) == 1) {

      Cidx <- .extract_column(C, idx_not_empty)
      Didx <- .extract_column(D, idx_not_empty)

    } else {

      Cidx <- C[, idx_not_empty]
      Didx <- D[, idx_not_empty]

    }

    C_B <- set_difference_single(Cidx@i, Cidx@p, Cidx@x,
                                 S@i, S@p, S@x,
                                 nrow(Cidx))

    D_B <- set_difference_single(Didx@i, Didx@p, Didx@x,
                                 S@i, S@p, S@x,
                                 nrow(Didx))

    idx_zeros <- Matrix::which(Matrix::colSums(D_B) == 0)

    if (length(idx_zeros) > 0) {

      C_B <- Matrix::Matrix(C_B[, -idx_zeros], sparse = TRUE)
      D_B <- Matrix::Matrix(D_B[, -idx_zeros], sparse = TRUE)

    }

    LHS <- cbind(C_B,
                 Matrix::Matrix(C[, -idx_not_empty], sparse = TRUE))
    RHS <- cbind(D_B,
                 Matrix::Matrix(D[, -idx_not_empty], sparse = TRUE))

  }

  L <- .generalization(LHS, RHS)
  L <- .composition(L$lhs, L$rhs)
  LHS <- L$lhs
  RHS <- L$rhs

  return(list(lhs = LHS, rhs = RHS, idx_removed = idx_removed))

}
