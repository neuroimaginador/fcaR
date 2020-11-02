.imp_to_basis <- function(LHS, RHS, attributes) {

  n <- ncol(LHS)

  for (i in seq(n)) {

    A <- Matrix::Matrix(LHS[, 1], sparse = TRUE)

    B <- Matrix::Matrix(RHS[, 1], sparse = TRUE)

    LHS <- Matrix::Matrix(LHS[, -1], sparse = TRUE)
    RHS <- Matrix::Matrix(RHS[, -1], sparse = TRUE)

    AUB <- .union(A, B)

    B <- .compute_closure(AUB, LHS, RHS,
                          attributes, reduce = FALSE)$closure

    LHS <- cbind(LHS, A)
    RHS <- cbind(RHS, B)

  }

  for (i in seq(n)) {

    A <- Matrix::Matrix(LHS[, 1], sparse = TRUE)

    B <- Matrix::Matrix(RHS[, 1], sparse = TRUE)

    LHS <- Matrix::Matrix(LHS[, -1], sparse = TRUE)
    RHS <- Matrix::Matrix(RHS[, -1], sparse = TRUE)

    A <- .compute_closure(A, LHS, RHS,
                          attributes, reduce = FALSE)$closure

    if (!(all(A == B))) {

      LHS <- cbind(LHS, A)
      RHS <- cbind(RHS, B)

    }

  }

  return(list(lhs = LHS, rhs = .difference2(RHS, LHS)))

}
