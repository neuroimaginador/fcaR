.recommend_attribute <- function(S, LHS, RHS, attribute_filter) {

  # Compute closure
  S <- .compute_closure(S, LHS, RHS)

  # Which attributes are seeked
  idx <- match(attribute_filter, rownames(LHS))

  # Return the recommendation for those attributes
  rec <- S[idx]
  names(rec) <- attribute_filter

  return(rec)

}
