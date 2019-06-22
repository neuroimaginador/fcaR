#' .subsethood <- function(B1, B2, I) {
#'
#'   ms1 <- .get_attribute_memberships(B1, I)
#'
#'   ms2 <- .get_attribute_memberships(B2, I)
#'
#'   min(.I.(ms1, ms2))
#'
#' }
