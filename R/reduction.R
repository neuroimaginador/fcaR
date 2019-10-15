.reduction <- function(LHS, RHS, attributes) {

  RHS <- .difference(RHS, LHS)

  return(list(lhs = LHS,
              rhs = RHS))

}
