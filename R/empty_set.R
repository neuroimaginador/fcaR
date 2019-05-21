# The (fuzzy) empty set
#' @import sets
.empty_attributes_set <- function(I) {

  attributes <- colnames(I)

  gset(support = attributes,
       memberships = rep(0, length(attributes)))

}
