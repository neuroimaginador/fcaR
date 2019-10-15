.concepts <- function(I, attributes, grades_set, verbose = FALSE) {

  empty <- Matrix(0, ncol = 1, nrow = ncol(I))
  Y <- Matrix(1, ncol = 1, nrow = ncol(I))

  intents <- list()

  n <- 1

  if (verbose) cat("First concept:\n")

  B <- .closure(empty, I)

  if (verbose) cat(.set_to_string(B, attributes), "\n")

  oldB <- B

  intents[[n]] <- B

  n <- n + 1

  exit_cond <- FALSE

  i <- ncol(I)
  imax <- ncol(I)

  while (!exit_cond) {

    B <- .next_closure(B, i, imax, grades_set,
                              closure_function = pryr::partial(.closure,
                                                               I = I))

    exit_cond <- all(B == oldB) || all(B == Y)

    intents[[n]] <- B

    if (verbose) {

      cat("New concept:\n")
      cat(.set_to_string(B, attributes), "\n")

    }

    n <- n + 1

    oldB <- B

  }

  concepts <- lapply(intents, function(b) {

    a <- .extent(b, I)

    list(a, b)

  })

  return(concepts)

}
