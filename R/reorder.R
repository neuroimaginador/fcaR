reorder <- function(LHS, RHS, attributes) {

  # Sizes of the LHS + size of the RHS
  # Remember: attributes are in rows, implications are
  # in columns.
  sizes <- colSums(LHS) + colSums(RHS)

  # Reorder the sizes:
  o <- order(sizes)

  # Return the reordered implications
  return(list(lhs = LHS %>% extract_columns(o),
              rhs = RHS %>% extract_columns(o)))

}
