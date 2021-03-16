meet <- function(subconcept_matrix, idx) {

  # Obtain the index of all subconcepts
  M <- subconcept_matrix %>% extract_columns(idx) %>% tSpM()
  #tSpM(subconcept_matrix)[idx, ]
  candidates <- which(colSums(M) == length(idx))

  if (length(candidates) > 1) {

    # If more than one, get their maximum
    M2 <- subconcept_matrix %>%
      extract_columns(candidates) %>%
      extract_rows(candidates)

    candidates <- candidates[which(colSums(M2) == length(candidates))]

  }

  return(candidates)

}

join <- function(subconcept_matrix, idx) {

  # Get the index of all superconcepts
  M <- subconcept_matrix %>% extract_rows(idx)
  candidates <- which(colSums(M) == length(idx))

  if (length(candidates) > 1) {

    # If more than one, obtain the minimum of
    # them:
    M2 <- subconcept_matrix %>%
      tSpM() %>%
      extract_columns(candidates) %>%
      extract_rows(candidates)

    candidates <- candidates[which(colSums(M2) == length(candidates))]

  }

  return(candidates)

}
