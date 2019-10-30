#' @import hasseDiagram
.draw_lattice <- function(L, I) {

  M <- .ordering_matrix(L, I)

  attributes <- rownames(I)
  objects <- colnames(I)

  labels <- sapply(L, function(l) .concept_to_string(l,
                                                     objects,
                                                     attributes))

  hasse(data = M,
        labels = labels,
        parameters = list(arrows = "backward"))

}
