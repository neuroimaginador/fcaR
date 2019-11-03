#' @import hasseDiagram
.draw_lattice <- function(L) {

  M <- .concept_order(L)

  attributes <- L[[1]]$get_intent()$get_attributes()
  objects <- L[[1]]$get_extent()$get_attributes()

  labels <- sapply(L, function(l) .concept_to_string(l,
                                                     objects,
                                                     attributes))

  hasse(data = M,
        labels = labels,
        parameters = list(arrows = "backward"))

}
