#' @import sets
.get_object_memberships <- function(A, I) {

  objects <- rownames(I)

  # Membership of each object in fuzzy set A
  ms_objects <- rep(0, length(objects))
  names(ms_objects) <- objects

  new_ms <- gset_memberships(A)

  if (length(new_ms) > 0) {

    names(new_ms) <- sapply(A, function(s) s)
    ms_objects[names(new_ms)] <- new_ms

  }

  return(ms_objects)

}

#' @import sets
.get_attribute_memberships <- function(B, I) {

  attributes <- colnames(I)

  ms_attr <- .get_grades(B, attributes)

  return(ms_attr)

}

#' @import sets
.get_grades <- function(B, attributes) {

  ms_attr <- rep(0, length(attributes))
  names(ms_attr) <- attributes

  new_ms <- gset_memberships(B)

  if (length(new_ms) > 0) {

    names(new_ms) <- sapply(B, function(s) s)
    ms_attr[names(new_ms)] <- new_ms

  }

  return(ms_attr)

}
