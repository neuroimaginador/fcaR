#' Computes the Duquenne-Guigues basis using optimized Ganter's algorithm for crisp data.
.get_dgbasis_binary_opt <- function(I,
                                    imp_basis = NULL,
                                    verbose = FALSE) {

  # close is the closure wrt implications
  # aclose is .closure_sparse

  n_attributes <- ncol(I)
  attributes <- colnames(I)
  empty <- Matrix(0,
                  ncol = 1,
                  nrow = n_attributes,
                  sparse = TRUE)

  if (!is.null(imp_basis)) {

    LHS <- as(Matrix(t(imp_basis$lhs), sparse = TRUE), "dgCMatrix")
    RHS <- as(Matrix(t(imp_basis$rhs), sparse = TRUE), "dgCMatrix")

    n_implications_basis <- ncol(LHS)

  } else {

    LHS <- NULL
    RHS <- NULL

    n_implications_basis <- c()

  }

  on.exit({

    if (length(n_implications_basis) > 0) {

      LHS <- LHS[, -seq(n_implications_basis)]
      RHS <- RHS[, -seq(n_implications_basis)]

    }

    DGbasis <- implication_set$new(name = "DGbasis",
                                   attributes = colnames(I),
                                   lhs = LHS, rhs = RHS)

    save_in_fca_env(DGbasis)

  })

  A <- empty

  implication_count <- 0
  intents <- list()
  n_intents <- 1
  intents[[1]] <- A
  n_intents <- n_intents + 1

  i <- n_attributes
  imax <- n_attributes

  while (sum(A) < n_attributes) {

    B <- .closure_sparse(A, I)

    rhs <- B
    rhs[A >= B] <- 0

    if (sum(rhs) == 0) {

      intents[[n_intents]] <- A

      n_intents <- n_intents + 1

    }

    # B-A is not empty -> add implication A -> B-A
    if (sum(rhs) > 0) {

      if (verbose) {

        cat("Added implication to basis:\n")
        cat(.implication_to_string(lhs = A, rhs = rhs, attributes), "\n")

      }

      # Add the A -> B-A implication
      if (is.null(LHS)) {

        LHS <- Matrix(A, sparse = TRUE)

      } else {

        LHS <- add_col(LHS, A)

      }

      if (is.null(RHS)) {

        RHS <- Matrix(rhs, sparse = TRUE)

      } else {

        RHS <- add_col(RHS, rhs)

      }

      implication_count <- implication_count + 1

      if (implication_count %% 10 == 0) {

        print(implication_count)

      }

    }

    # There is any attribute with index <= i in the closure
    if (sum(rhs[1:i]) > 0) {

      A[i:imax] <- 0

    } else {

      # We have obtained the last closed set
      if (sum(B) == n_attributes) {

        if (length(n_implications_basis) > 0) {

          LHS <- LHS[, -seq(n_implications_basis)]
          RHS <- RHS[, -seq(n_implications_basis)]

        }

        DGbasis <- implication_set$new(name = "DGbasis",
                                       attributes = colnames(I),
                                       lhs = LHS, rhs = RHS)

        return(DGbasis)

      }

      A <- B
      i <- imax

    }

    for (j in seq(i, 1)) {

      if (A[j] > 0) {

        A[j] <- 0

      } else {

        A2 <- A
        A2[j] <- 1

        B <- .compute_closure_binary(A2, LHS, RHS)

        B_A <- B
        B_A[A >= B] <- 0

        if (B_A@i[1] + 1 >= j) {

          A <- B
          i <- j

          break

        }

      }

    }

  }

  if (length(n_implications_basis) > 0) {

    LHS <- LHS[, -seq(n_implications_basis)]
    RHS <- RHS[, -seq(n_implications_basis)]

  }

  DGbasis <- implication_set$new(name = "DGbasis",
                                 attributes = colnames(I),
                                 lhs = LHS, rhs = RHS)

  return(DGbasis)

}
