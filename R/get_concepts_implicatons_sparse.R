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

  intents <- list()
  DGbasis <- implication_set$new(name = "DGbasis",
                                 attributes = colnames(I))

  A <- .closure_sparse(empty, I)

  n_intents <- 1
  intents[[1]] <- A
  n_intents <- n_intents + 1

  if (sum(A) > 0) {

    # Add the empty -> A implication
    DGbasis$add_implication(lhs = empty, rhs = A)

    if (verbose) {

      cat("Added {} to A implication\n")

    }

  }

  exit_cond <- FALSE

  while (!exit_cond) {

    A <- .next_closure_sparse(A, I, grades_set,
                              closure_function = DGbasis$compute_closure)

    B <- .closure_sparse(A, I)

    if (all(A == B)) {

      intents[[n_intents]] <- A
      n_intents <- n_intents + 1

      if (verbose) {

        print("New concept:")
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
      DGbasis$add_implication(lhs = A, rhs = rhs)

    }

    exit_cond <- all(A == Y)

  }

  concepts <- lapply(intents, function(b) {

    a <- .extent_sparse(b, I)

    list(a, b)

  })

  return(list(concepts = concepts, implications = DGbasis))

}
