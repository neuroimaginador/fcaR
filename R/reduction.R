.reduction <- function(LHS, RHS, attributes) {

  RHS <- .difference2(RHS, LHS)

  return(list(lhs = LHS,
              rhs = RHS))

}
