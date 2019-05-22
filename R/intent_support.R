.intent_support <- function(B, I) {

  # Compute B'
  A <- .extent(B, I)

  # Cardinal of the extent
  ms <- .get_object_memberships(A, I)

  # Support
  support <- sum(ms) / length(ms)

  return(support)

}
