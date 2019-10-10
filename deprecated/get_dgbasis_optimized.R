#' Computes the Duquenne-Guigues basis using optimized Ganter's algorithm.
.get_dgbasis_opt <- function(I,
                             grades_set = sort(unique(as.vector(I))),
                             verbose = FALSE) {

  # close is the closure wrt implications
  # aclose is .closure_sparse

  n_attributes <- ncol(I)
  attributes <- colnames(I)
  empty <- Matrix(0,
                  ncol = 1,
                  nrow = n_attributes,
                  sparse = TRUE)

  Y <- Matrix(1,
              ncol = 1,
              nrow = n_attributes,
              sparse = TRUE)

  LHS <- NULL

  RHS <- NULL

  on.exit({

    DGbasis <- implication_set$new(name = "DGbasis",
                                   attributes = colnames(I),
                                   lhs = LHS, rhs = RHS)

    assign("DGbasis", DGbasis, envir = globalenv())

  })

  intents <- list()
  # DGbasis <- implication_set$new(name = "DGbasis",
  #                                attributes = colnames(I))

  A <- empty

  implication_count <- 0
  n_intents <- 1
  intents[[1]] <- A
  n_intents <- n_intents + 1

  exit_cond <- FALSE

  i <- n_attributes
  imax <- n_attributes

  while (sum(A) < n_attributes) {

    B <- .closure_sparse(A, I)

    rhs <- B
    rhs[A >= B] <- 0

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

        DGbasis <- implication_set$new(name = "DGbasis",
                                       attributes = colnames(I),
                                       lhs = LHS, rhs = RHS)

        return(DGbasis)

      }

      A <- B
      i <- imax

    }

    .closure_implications <- function(S) {

      .compute_closure2(S, LHS, RHS)

    }

    L <- .next_closure_opt(A, i, imax,
                           grades_set,
                           closure_function = .closure_implications)

    A <- L$A
    i <- L$i

  }


}

