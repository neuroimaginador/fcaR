.reduceAinB <- function(lhs, rhs) {

  new_rhs <- rhs - lhs

  imp <- implication$new(lhs = lhs, rhs = new_rhs)

  return(imp)

}
