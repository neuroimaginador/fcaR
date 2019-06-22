#' @import hasseDiagram
.draw_Hasse <- function(L, I) {

  M <- .get_ordering_matrix_sparse(L, I)

  attributes <- rownames(I)
  objects <- colnames(I)

  labels <- sapply(L, function(l) .sparse_concept_to_string(l,
                                                            objects,
                                                            attributes))

  hasse(data = M,
        labels = labels,
        parameters = list(arrows = "backward"))

}
