#' @import sets
.get_fuzzy_concepts <- function(I, grades_set, verbose = FALSE) {

  empty <- .empty_attributes_set(I)
  Y <- gset(support = colnames(I))

  intents <- list()

  n <- 1

  if (verbose) cat("First concept:\n")

  B <- .closure(empty, I)

  if (verbose) print(B)

  oldB <- B

  intents[[n]] <- B

  n <- n + 1

  exit_cond <- FALSE

  while (!exit_cond) {

    B <- .next_closure(B, I, grades_set,
                       closure_function = pryr::partial(.closure,
                                                        I = I))

    exit_cond <- (B == oldB) || (B == Y)

    intents[[n]] <- B

    if (verbose) {

      cat("New concept:\n")
      print(B)

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
