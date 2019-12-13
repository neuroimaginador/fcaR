# Returns true if concept C1 is subconcept of C2
`%<=%` <- function(C1, C2) {

  if (inherits(C1, "SparseConcept") &
      inherits(C2, "SparseConcept")) {

    return(all(C1$get_extent()$get_vector() <= C2$get_extent()$get_vector()))

  }

  if (inherits(C1, "SparseSet") |
      inherits(C2, "SparseSet")) {

    return(all(C1$get_vector() <= C2$get_vector()))

  }

  stop("Only implemented for SparseConcepts and SparseSets.\n",
       call. = FALSE)

}


`%==%` <- function(C1, C2) {

  # Equality of sets/concepts
  if (inherits(C1, "SparseConcept") &
      inherits(C2, "SparseConcept")) {

    return(all(C1$get_extent()$get_vector() == C2$get_extent()$get_vector()))

  }

  if (inherits(C1, "SparseSet") |
      inherits(C2, "SparseSet")) {

    return(all(C1$get_vector() == C2$get_vector()))

  }

  stop("Only implemented for SparseConcepts and SparseSets.\n",
       call. = FALSE)

}

`%-%` <- function(S1, S2) {

  # Fuzzy set difference
  if (inherits(S1, "SparseSet") |
      inherits(S2, "SparseSet")) {

    S <- SparseSet$new(attributes = S1$get_attributes())
    A <- S1$get_vector()
    B <- S2$get_vector()
    A[B > A] <- 0
    idx <- which(A > 0)
    S$assign(attributes = S$get_attributes()[idx], values = A[idx])

    return(S)

  }

  stop("Only implemented for SparseSets.\n",
       call. = FALSE)

}
