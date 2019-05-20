#' @import sets
.direct_sum <- function(B, attr_i, grade_i, I) {

  attributes <- colnames(I)

  a_i <- which(attributes == attr_i)

  if (a_i == 1) {

    lowerB <- .empty_attributes_set(I)

  } else {

    lower_attr <- attributes[1:(a_i - 1)]

    ms <- .get_attribute_memberships(B, I)[lower_attr]

    lowerB <- gset(support = lower_attr,
                   memberships = ms)

  }

  unitB <- gset(support = attr_i,
                memberships = grade_i)

  the_sum <- gset_union(gset_intersection(B, lowerB),
                        unitB)

  the_sum <- .closure(the_sum, I)

  return(the_sum)

}
