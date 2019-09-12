.get_concepts_implications_sparse <- function(I,
                                              grades_set,
                                              verbose = FALSE) {

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

  A <- .closure_sparse(empty, I)

  implication_count <- 0
  n_intents <- 1
  intents[[1]] <- A
  n_intents <- n_intents + 1

  if (sum(A) > 0) {

    # Add the empty -> A implication

    LHS <- Matrix(empty, sparse = TRUE)
    RHS <- Matrix(A, sparse = TRUE)

    implication_count <- implication_count + 1

    if (verbose) {

      cat("Added {} to A implication\n")

    }

  }

  exit_cond <- FALSE

  i <- n_attributes
  imax <- n_attributes

  while (!exit_cond) {

    A <- .next_closure_sparse_espec(A, i, imax,
                                    grades_set, LHS, RHS,
                                    attributes)


    B <- .closure_sparse(A, I)

    if (all(A == B)) {

      intents[[n_intents]] <- A
      n_intents <- n_intents + 1

      if (verbose) {

        cat("Added concept:\n")
        cat(.sparse_set_to_string(A, attributes), "\n")

      }

    } else {

      rhs <- B
      rhs[A >= B] <- 0

      if (verbose) {

        cat("Added implication to basis:\n")
        # print(A)
        cat(.implication_to_string(lhs = A, rhs = rhs, attributes), "\n")

      }

      # Add the A -> B\A implication

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

      if (implication_count %% 10 == 0)   print(implication_count)

    }

    exit_cond <- all(A == Y)

  }

  DGbasis <- implication_set$new(name = "DGbasis",
                                 attributes = colnames(I),
                                 lhs = LHS, rhs = RHS)

  concepts <- lapply(intents, function(b) {

    a <- .extent_sparse(b, I)

    list(a, b)

  })

  return(list(concepts = concepts, implications = DGbasis))

}
