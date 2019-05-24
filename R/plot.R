.gset_to_string <- function(S) {

  strwrap(format(S), width = 5000, exdent = 1L)

}

.concept_to_string <- function(C) {

  A <- .gset_to_string(C[[1]])
  B <- .gset_to_string(C[[2]])

  return(paste0("[", A, ", ", B, "]"))

}

#' @import hasseDiagram
.draw_Hasse <- function(L, I) {

  M <- .get_ordering_matrix(L, I)

  labels <- sapply(L, .concept_to_string)

  hasse(data = t(M),
        labels = labels,
        parameters = list(arrows = "backward"))

}
