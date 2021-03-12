combine_implications <- function(imps1, imps2) {

  if (is.null(imps1)) {

    return(imps2)

  }

  att1 <- imps1$get_attributes()
  att2 <- imps2$get_attributes()
  atts <- sort(unique(c(att1, att2)))


  if (imps1$cardinality() + imps2$cardinality() == 0) {

    return(ImplicationSet$new(attributes = atts))

  }

  if (imps1$cardinality() > 0) {

    lhs1 <- imps1$get_LHS_matrix() %>% tSpM()
    rhs1 <- imps1$get_RHS_matrix() %>% tSpM()

    lhsA <- zeroSpM(nrow = imps1$cardinality(),
                    ncol = length(atts))
    rhsA <- rlang::env_clone(lhsA)

    id1 <- match(att1, atts)
    lhsA %>% substitute_columns(id1, lhs1)
    rhsA %>% substitute_columns(id1, rhs1)

    lhsA <- tSpM(lhsA)
    rhsA <- tSpM(rhsA)

  } else {

    lhsA <- rhsA <- NULL

  }

  if (imps2$cardinality() > 0) {

    lhs2 <- imps2$get_LHS_matrix() %>% tSpM()
    rhs2 <- imps2$get_RHS_matrix() %>% tSpM()

    lhsB <- zeroSpM(nrow = imps2$cardinality(),
                    ncol = length(atts))
    rhsB <- rlang::env_clone(lhsB)

    id2 <- match(att2, atts)
    lhsB %>% substitute_columns(id2, lhs2)
    rhsB %>% substitute_columns(id2, rhs2)

    lhsB <- tSpM(lhsB)
    rhsB <- tSpM(rhsB)

  } else {

    lhsB <- rhsB <- NULL

  }

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
