.imp_to_basis_bg <- function(lhs_bg, rhs_bg, LHS, RHS, attributes) {

  n <- ncol(LHS)

  for (i in seq(n)) {

    A <- Matrix::Matrix(LHS[, 1], sparse = TRUE)

    B <- Matrix::Matrix(RHS[, 1], sparse = TRUE)

    LHS <- Matrix::Matrix(LHS[, -1], sparse = TRUE)
    RHS <- Matrix::Matrix(RHS[, -1], sparse = TRUE)

    AUB <- .union(A, B)

    LHS_clos <- cbind(lhs_bg, LHS)
    RHS_clos <- cbind(rhs_bg, RHS)

    warning(str(AUB), immediate. = TRUE)
    warning(str(LHS_clos), inmediate. = TRUE)
    warning(str(RHS_clos), inmediate. = TRUE)

    B <- .compute_closure(AUB, LHS_clos, RHS_clos,
                          attributes, reduce = TRUE)$closure

    LHS <- cbind(LHS, A)
    RHS <- cbind(RHS, B)

  }

  for (i in seq(n)) {

    A <- Matrix::Matrix(LHS[, 1], sparse = TRUE)

    B <- Matrix::Matrix(RHS[, 1], sparse = TRUE)

    LHS <- Matrix::Matrix(LHS[, -1], sparse = TRUE)
    RHS <- Matrix::Matrix(RHS[, -1], sparse = TRUE)

    LHS_clos <- cbind(lhs_bg, LHS)
    RHS_clos <- cbind(rhs_bg, RHS)

    warning(str(AUB), immediate. = TRUE)
    warning(str(LHS_clos), inmediate. = TRUE)
    warning(str(RHS_clos), inmediate. = TRUE)

    A <- .compute_closure(A, LHS_clos, RHS_clos,
                          attributes, reduce = TRUE)$closure

    if (!(all(A == B))) {

      LHS <- cbind(LHS, A)
      RHS <- cbind(RHS, B)

    }

  }

  L <- .Rsimplification_bg(lhs_bg = lhs_bg,
                           rhs_bg = rhs_bg,
                           LHS = LHS,
                           RHS = RHS)
  L <- .simplification_bg(lhs_bg = lhs_bg,
                          rhs_bg = rhs_bg,
                          lhs = L$lhs,
                          rhs = L$rhs)
  LHS <- L$lhs
  RHS <- L$rhs

  return(list(lhs = LHS, rhs = .difference2(RHS, LHS)))

}
