.matrix_to_concepts <- function(M_ext, M_int,
                               objects, attributes) {

  n_concepts <- ncol.SpM(M_ext)
  L <- list()

  for (i in seq(n_concepts)) {

    x <- M_ext %>% extract_columns(i)

    s <- M_int %>% extract_columns(i)

    extent <- SparseSet$new(attributes = objects,
                             M = x)
    intent <- SparseSet$new(attributes = attributes,
                             M = s)

    L[[i]] <- SparseConcept$new(extent = extent,
                                 intent = intent)

  }

  return(L)

}
