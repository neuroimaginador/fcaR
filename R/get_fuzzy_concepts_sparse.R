.get_fuzzy_concepts_sparse <- function(I, grades_set, verbose = FALSE) {

  empty <- Matrix(0, ncol = 1, nrow = ncol(I))
  Y <- Matrix(1, ncol = 1, nrow = ncol(I))

  intents <- list()

  n <- 1

  if (verbose) cat("First concept:\n")

  B <- .closure_sparse(empty, I)

  if (verbose) cat(.sparse_set_to_string(B, colnames(I)), "\n")

  oldB <- B

  intents[[n]] <- B

  n <- n + 1

  exit_cond <- FALSE

  i <- ncol(I)
  imax <- ncol(I)

  while (!exit_cond) {

    B <- .next_closure_sparse(B, i, imax, grades_set,
                              closure_function = pryr::partial(.closure_sparse,
                                                               I = I))

    exit_cond <- all(B == oldB) || all(B == Y)

    intents[[n]] <- B

    if (verbose) {

      cat("New concept:\n")
      cat(.sparse_set_to_string(B), "\n")

    }

    n <- n + 1

    oldB <- B

  }

  concepts <- lapply(intents, function(b) {

    a <- .extent_sparse(b, I)

    list(a, b)

  })

  return(concepts)

}
