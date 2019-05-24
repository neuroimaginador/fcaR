#' @import sets
.get_concepts_implications <- function(I,
                                       grades_set,
                                       verbose = FALSE) {

  empty <- .empty_attributes_set(I)
  Y <- gset(support = colnames(I))

  intents <- list()
  DGbasis <- implication_set$new("DGbasis")

  A <- .closure(empty, I)

  n_intents <- 1
  intents[[1]] <- A
  n_intents <- n_intents + 1

  if (!gset_is_empty(A)) {

    # Add the empty -> A implication
    DGbasis$add_implication(

      implication$new(lhs = empty,
                      rhs = A)

    )

    if (verbose) {

      cat("Added {} to A implication\n")

    }

  }

  exit_cond <- FALSE

  while (!exit_cond) {

    A <- .next_closure(A, I, grades_set,
                       closure_function = DGbasis$compute_closure)

    B <- .closure(A, I)

    if (gset_is_equal(A, B)) {

      intents[[n_intents]] <- A
      n_intents <- n_intents + 1

      if (verbose) {

        print("New concept:")
        print(A)

      }

    } else {

      rhs <- gset_difference(B, A)

      imp <- implication$new(lhs = A,
                             rhs = rhs)

      if (verbose) {

        cat("Added implication to basis:\n")
        print(imp)

      }

      # Add the A -> B\A implication
      DGbasis$add_implication(imp)

    }

    exit_cond <- A == Y


  }

  concepts <- lapply(intents, function(b) {

    a <- .extent(b, I)

    list(a, b)

  })

  return(list(concepts = concepts, implications = DGbasis))

}
