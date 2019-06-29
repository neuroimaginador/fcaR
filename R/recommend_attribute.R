.recommend_attribute <- function(S, LHS, RHS, attribute_filter, attributes) {

  # Compute closure
  S <- .compute_closure2(S, LHS, RHS)

  # Which attributes are seeked
  idx <- match(attribute_filter, attributes)

  # Return the recommendation for those attributes
  rec <- S[idx]
  names(rec) <- attribute_filter

  return(rec)

}
