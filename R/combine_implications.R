combine_implications <- function(imps1, imps2) {

  if (is.null(imps1)) {

    return(imps2)

  }

  att1 <- imps1$get_attributes()
  lhs1 <- imps1$get_LHS_matrix()
  rhs1 <- imps1$get_RHS_matrix()
  att2 <- imps2$get_attributes()
  lhs2 <- imps2$get_LHS_matrix()
  rhs2 <- imps2$get_RHS_matrix()

  atts <- sort(unique(c(att1, att2)))
  lhsA <- zeroSpM(ncol = imps1$cardinality(),
                  nrow = length(atts))
  rhsA <- lhsA

  lhsB <- zeroSpM(ncol = imps2$cardinality(),
                  nrow = length(atts))
  rhsB <- lhsB

  id1 <- match(att1, atts)
  lhsA %>% substitute_columns(id1, lhs1)
  rhsA %>% substitute_columns(id1, rhs1)

  id2 <- match(att2, atts)
  lhsB %>% substitute_columns(id2, lhs2)
  rhsB %>% substitute_columns(id2, rhs2)

  ImplicationSet$new(attributes = atts,
                     lhs = cbindSpM(lhsA, lhsB),
                     rhs = cbindSpM(rhsA, rhsB))

}

reorder_attributes <- function(imps, attributes) {

  lhs <- imps$get_LHS_matrix()
  rhs <- imps$get_RHS_matrix()
  atts <- imps$get_attributes()

  id <- match(attributes, atts)

  lhs <- lhs %>% extract_rows(id)
  rhs <- rhs %>% extract_rows(id)

  ImplicationSet$new(attributes = attributes,
                     lhs = lhs,
                     rhs = rhs)

}
