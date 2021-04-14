.compute_closure <- function(S, LHS, RHS, attributes,
                             reduce = FALSE, verbose = FALSE,
                             is_direct = FALSE) {

  if (is.null(LHS) || (ncol.SpM(LHS) == 0)) {

    return(list(closure = S,
                implications = list(lhs = LHS,
                                    rhs = RHS)))

  }

  # Which are the rules applicable to the set S?
  S_subsets <- tSpM(subsetSpM(LHS, S))

  # idx_subsets <- which(S_subsets)
  idx_subsets <- S_subsets$pi

  do_not_use <- rep(FALSE, ncol.SpM(LHS))

  passes <- 0

  # While there are applicable rules, apply!!
  while (length(idx_subsets) > 0) {

    passes <- passes + 1
    if (verbose) cat("Pass #", passes, "\n")

    A <- RHS %>% extract_columns(idx_subsets)


    S <- cbindSpM(A, S) %>%
      flattenSpM()

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


    if (is.null(LHS) || (ncol.SpM(LHS) == 0)) {

      return(list(closure = S,
                  implications = list(lhs = LHS,
                                      rhs = RHS)))
    }

    if (!is_direct) {

      S_subsets <- tSpM(subsetSpM(LHS, S))

      idx_subsets <- S_subsets$pi
      idx_subsets <- setdiff(idx_subsets, which(do_not_use))

      if (verbose) {

        print(idx_subsets)
        print(SparseSet$new(attributes = attributes,
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

  # browser()

  # Equivalence II
  subsets <- subsetSpM(RHS, S) %>% tSpM()
  idx_subsets <- subsets$pi

  idx_removed <- list()

  if (length(idx_subsets) > 0) {

    idx_removed[[1]] <- idx_subsets

    LHS <- LHS %>% remove_columns(idx_subsets)
    RHS <- RHS %>% remove_columns(idx_subsets)

  }

  if (ncol.SpM(LHS) == 0) {

    return(list(lhs = NULL, rhs = NULL))

  }

  # Equivalence III
  C <- LHS
  D <- RHS

  CD <- unionSpM(LHS, RHS)

  intersections <- tSpM(.intersection(x = S, y = CD))
  idx_not_empty <- which(colSums(intersections) > 0)

  if (length(idx_not_empty) > 0) {

    Cidx <- C %>% extract_columns(idx_not_empty)
    Didx <- D %>% extract_columns(idx_not_empty)

    C_B <- differenceSpM(Cidx, S)
    D_B <- differenceSpM(Didx, S)


    idx_zeros <- which(colSums(D_B) == 0)

    if (length(idx_zeros) > 0) {

      idx_removed <- c(idx_removed, list(idx_zeros))

      C_B <- C_B %>% remove_columns(idx_zeros)
      D_B <- D_B %>% remove_columns(idx_zeros)

    }

    idx_removed <- c(idx_removed, list(idx_not_empty))

    C <- remove_columns(C, idx_not_empty)
    D <- remove_columns(D, idx_not_empty)


    if (C$dim[2] > 0) {

      LHS <- cbindSpM(C_B, C)
      RHS <- cbindSpM(D_B, D)

    } else {

      LHS <- C_B
      RHS <- D_B

    }

  }

  return(list(lhs = LHS, rhs = RHS, idx_removed = idx_removed))

}
