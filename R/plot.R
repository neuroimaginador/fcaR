.gset_to_string <- function(S) {

  strwrap(format(S), width = 5000, exdent = 1L)

}

.sparse_set_to_string <- function(S, attributes) {

  idx <- which(S > 0)

  if (length(idx) > 0) {

    A <- S[idx]
    att <- attributes[idx]

    paste0("{",
           stringr::str_flatten(paste0(att, " [", A, "]"),
                                collapse = ", "), "}")

  } else {

    "{}"

  }

}

.sparse_concept_to_string <- function(C, attributes) {

  A <- .sparse_set_to_string(C[[1]], attributes)
  B <- .sparse_set_to_string(C[[2]], attributes)

  return(paste0("[", A, ", ", B, "]"))

}

.concept_to_string <- function(C) {

  A <- .gset_to_string(C[[1]])
  B <- .gset_to_string(C[[2]])

  return(paste0("[", A, ", ", B, "]"))

}

#' @import hasseDiagram
.draw_Hasse <- function(L, I) {

  M <- .get_ordering_matrix_sparse(L, I)

  attributes <- colnames(I)

  labels <- sapply(L, function(l) .sparse_concept_to_string(l, attributes))

  hasse(data = M,
        labels = labels,
        parameters = list(arrows = "backward"))

}
