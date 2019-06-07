.reduce_lhs_rhs <- function(LHS, RHS) {

  LHS <- as.matrix(LHS)
  RHS <- as.matrix(RHS)

  RHS <- apply_F_elementwise(x = RHS,
                             y = LHS,
                             type = "set_diff")

  return(list(lhs = Matrix(LHS, sparse = TRUE),
              rhs = Matrix::Matrix(RHS, sparse = TRUE)))

}
