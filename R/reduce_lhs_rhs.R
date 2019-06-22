.reduce_lhs_rhs <- function(LHS, RHS, attributes) {

  RHS <- sparse_set_difference(RHS, LHS)

  return(list(lhs = LHS,
              rhs = RHS))

}
