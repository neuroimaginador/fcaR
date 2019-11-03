.concept_order <- function(L) {

  extents <- lapply(L, function(l) l$get_extent()$get_vector())

  extents <- Reduce(cbind, extents)

  M <- .subset(extents)

  return(as.matrix(t(M)))

}
