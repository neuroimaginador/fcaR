fast_ganter_algo <- function(I, expanded_grades_set) {

  n_attributes <- ncol(I)
  grades_set <- sort(unique(as.vector(I)))
  grades_set <- grades_set[grades_set > 0]

  on.exit({

    # intents <- lapply(L$concepts, function(r) Matrix(r, sparse = TRUE))

    LHS <- do.call(cbind, L$LHS)
    RHS <- do.call(cbind, L$RHS)

    DGbasis <- implication_set$new(name = "DGbasis",
                                   attributes = colnames(I),
                                   lhs = LHS, rhs = RHS)

    return(list(concepts = L$concepts,
               implications = DGbasis))

  })

  L <- ganters_algorithm_implications_struct(I,
                                      expanded_grades_set,
                                      n_attributes)

  # intents <- lapply(L$concepts, function(r) Matrix(r, sparse = TRUE))

  LHS <- do.call(cbind, L$LHS)
  RHS <- do.call(cbind, L$RHS)

  LHS <- Matrix(LHS, sparse = TRUE)
  RHS <- Matrix(RHS, sparse = TRUE)

  DGbasis <- implication_set$new(name = "DGbasis",
                                 attributes = colnames(I),
                                 lhs = LHS, rhs = RHS)

  # concepts <- lapply(intents, function(b) {
  #
  #   a <- .extent_sparse(b, I)
  #
  #   list(a, b)
  #
  # })

  return(list(concepts = L$concepts,
              implications = DGbasis,
              timer = L$timer))

}
