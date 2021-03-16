.clean <- function(LHS, RHS) {

  idx <- which(colSums(RHS) == 0)

  if (length(idx) > 0) {

    LHS <- LHS %>% remove_columns(idx)
    RHS <- RHS %>% remove_columns(idx)

  }

  return(list(lhs = LHS, rhs = RHS))

}
