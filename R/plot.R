.concept_to_string <- function(C) {

  A <- strwrap(format(C[[1]]), width = 5000, exdent = 1L)
  B <- strwrap(format(C[[2]]), width = 5000, exdent = 1L)

  return(paste0("[", A, ", ", B, "]"))

}

#' @import hasseDiagram
.draw_Hasse <- function(L, I) {

  L2 <- L
  L2[[length(L) + 1]] <- list(.empty_attributes_set(I), 0)
  M <- .get_ordering_matrix(L2, I)

  labels <- c(sapply(L, .concept_to_string), "{}")

  hasse(data = t(M),
        labels = labels,
        parameters = list(arrows = "backward"))

}
