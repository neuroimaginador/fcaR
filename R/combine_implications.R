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

    lhs1 <- imps1$get_LHS_matrix()
    rhs1 <- imps1$get_RHS_matrix()

    id1 <- match(att1, atts)

    lhsA <- Matrix::spMatrix(nrow = length(atts),
                             ncol = imps1$cardinality())
    rhsA <- Matrix::spMatrix(nrow = length(atts),
                             ncol = imps1$cardinality())

    lhsA[id1, ] <- lhs1
    rhsA[id1, ] <- rhs1

  } else {

    lhsA <- rhsA <- NULL

  }

  if (imps2$cardinality() > 0) {

    lhs2 <- imps2$get_LHS_matrix()
    rhs2 <- imps2$get_RHS_matrix()

    id2 <- match(att2, atts)

    lhsB <- Matrix::spMatrix(nrow = length(atts),
                             ncol = imps2$cardinality())
    rhsB <- Matrix::spMatrix(nrow = length(atts),
                             ncol = imps2$cardinality())

    lhsB[id2, ] <- lhs2
    rhsB[id2, ] <- rhs2

  } else {

    lhsB <- rhsB <- NULL

  }

  ImplicationSet$new(attributes = atts,
                     lhs = cbind(lhsA, lhsB),
                     rhs = cbind(rhsA, rhsB))

}

reorder_attributes <- function(imps, attributes) {

  lhs <- imps$get_LHS_matrix()
  rhs <- imps$get_RHS_matrix()
  atts <- imps$get_attributes()

  # browser()

  LHS <- Matrix::spMatrix(nrow = length(attributes),
                          ncol = imps$cardinality())
  RHS <- Matrix::spMatrix(nrow = length(attributes),
                          ncol = imps$cardinality())

  id <- match(attributes, atts)

  LHS[!is.na(id), ] <- lhs[id[!is.na(id)], ]
  RHS[!is.na(id), ] <- rhs[id[!is.na(id)], ]

  ImplicationSet$new(attributes = attributes,
                     lhs = LHS,
                     rhs = RHS)

}
