meet <- function(subconcept_matrix, idx) {

  # Obtain the index of all subconcepts
  M <- Matrix::t(subconcept_matrix)[idx, ]
  candidates <- Matrix::which(Matrix::colSums(M) == length(idx))

  if (length(candidates) > 1) {

    # If more than one, get their maximum
    M2 <- subconcept_matrix[candidates, candidates]

    candidates <- candidates[Matrix::which(Matrix::colSums(M2) == length(candidates))]

  }

  return(candidates)

}

join <- function(subconcept_matrix, idx) {

  # Get the index of all superconcepts
  M <- subconcept_matrix[idx, ]
  candidates <- Matrix::which(Matrix::colSums(M) == length(idx))

  if (length(candidates) > 1) {

    # If more than one, obtain the minimum of
    # them:
    M2 <- Matrix::t(subconcept_matrix)[candidates, candidates]

    candidates <- candidates[Matrix::which(Matrix::colSums(M2) == length(candidates))]

  }

  return(candidates)

}
