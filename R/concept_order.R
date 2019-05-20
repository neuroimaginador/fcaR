#' @import sets
.is_subconcept <- function(A1, A2, I) {

  gset_is_subset(A1, A2)

}

.get_ordering_matrix <- function(L, I) {

  n_concepts <- length(L)

  M <- matrix(FALSE,
              nrow = n_concepts,
              ncol = n_concepts)

  for (idx_A1 in seq(n_concepts)) {

    for (idx_A2 in seq(n_concepts)) {

      M[idx_A1, idx_A2] <- .is_subconcept(A1 = L[[idx_A1]][[1]],
                                         A2 = L[[idx_A2]][[1]],
                                         I)
    }

  }

  return(M)

}
