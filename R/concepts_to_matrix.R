.matrix_to_concepts <- function(M_ext, M_int,
                               objects, attributes) {

  n_concepts <- ncol(M_ext)
  L <- list()

  for (i in seq(n_concepts)) {

    x <- .extract_column(M_ext, i)

    s <- .extract_column(M_int, i)

    extent <- SparseSet$new(attributes = objects,
                             M = x)
    intent <- SparseSet$new(attributes = attributes,
                             M = s)

    L[[i]] <- SparseConcept$new(extent = extent,
                                 intent = intent)

  }

  return(L)

}
