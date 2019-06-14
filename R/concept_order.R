.is_subconcept_sparse <- function(A1, A2) {

  all(A1 <= A2)

}

.get_ordering_matrix_sparse <- function(L, I) {

  n_concepts <- length(L)

  M <- matrix(FALSE,
              nrow = n_concepts,
              ncol = n_concepts)

  for (idx_A1 in seq(n_concepts)) {

    for (idx_A2 in seq(n_concepts)) {

      M[idx_A1, idx_A2] <- .is_subconcept_sparse(A1 = L[[idx_A1]][[1]],
                                                 A2 = L[[idx_A2]][[1]])
    }

  }

  return(M)

}
