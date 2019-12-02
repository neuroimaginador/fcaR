.concepts_to_matrix <- function(L) {

  intents <- lapply(L, function(l) l$get_intent()$get_vector())
  intents <- Reduce(add_col, intents)

  extents <- lapply(L, function(l) l$get_extent()$get_vector())
  extents <- Reduce(add_col, extents)

  return(list(extents = extents, intents = intents))

}

.matrix_to_concepts <- function(M_ext, M_int,
                               objects, attributes) {

  n_concepts <- ncol(M_ext)
  L <- list()

  for (i in seq(n_concepts)) {

    x <- .extract_column(M_ext, i)

    s <- .extract_column(M_int, i)

    extent <- sparse_set$new(attributes = objects,
                             M = x)
    intent <- sparse_set$new(attributes = attributes,
                             M = s)

    L[[i]] <- sparse_concept$new(extent = extent,
                                 intent = intent)

  }

  return(L)

}
