#' @import sets
.get_fuzzy_concepts <- function(I, grades_set) {

  empty <- .empty_attributes_set(I)
  Y <- gset(support = colnames(I))

  intents <- list()

  n <- 1

  cat("First concept:\n")

  B <- .closure(empty, I)

  print(B)

  oldB <- B

  intents[[n]] <- B

  n <- n + 1

  exit_cond <- FALSE

  while (!exit_cond) {

    B <- .compute_next_intent(B, I, grades_set)

    exit_cond <- (B == oldB) || (B == Y)

    intents[[n]] <- B
    cat("New concept:\n")
    print(B)

    n <- n + 1

    oldB <- B

  }

  concepts <- lapply(intents, function(b) {

    a <- .extent(b, I)

    list(a, b)

  })

  return(concepts)

}
