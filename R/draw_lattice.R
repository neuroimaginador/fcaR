#' @import hasseDiagram
.draw_lattice <- function(L, object_names = TRUE) {

  M <- .concept_order(L)

  attributes <- L[[1]]$get_intent()$get_attributes()
  objects <- L[[1]]$get_extent()$get_attributes()

  if (object_names) {

    labels <- sapply(L, function(l) .concept_to_string(l,
                                                       objects,
                                                       attributes))

  } else {

    labels <- sapply(L, function(l) {
      v <- l$get_intent()
      .set_to_string(S = v$get_vector(),
                     attributes = v$get_attributes())
      })

  }

  labels <- sapply(labels, function(str) str_wrap(str, width = 20))

  labels <- lapply(labels, function(str) unlist(str_split(str, pattern = "\\n")))

  hasse(data = M,
        labels = labels,
        parameters = list(arrows = "backward"))

}
