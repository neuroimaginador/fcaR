# Returns true if concept C1 is subconcept of C2
`%<=%` <- function(C1, C2) {

  if (!inherits(C1, "SparseConcept") |
      !inherits(C2, "SparseConcept")) {

    stop("Only implemented for SparseConcepts.\n",
         call. = FALSE)

  }

  all(C1$get_extent()$get_vector() <= C2$get_extent()$get_vector())

}
