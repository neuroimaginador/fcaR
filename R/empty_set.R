# The (fuzzy) empty set
.empty_attributes_set <- function(I) {

  attributes <- colnames(I)

  gset(support = attributes,
       memberships = rep(0, length(attributes)))

}
