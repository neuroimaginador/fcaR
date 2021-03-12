.imp_to_basis <- function(LHS, RHS, attributes) {

  n <- ncol.SpM(LHS)

  for (i in seq(n)) {

    A <- LHS %>% extract_columns(1)
    B <- RHS %>% extract_columns(1)

    LHS <- LHS %>% remove_columns(1)
    RHS <- RHS %>% remove_columns(1)

    AUB <- unionSpM(A, B)

    B <- .compute_closure(AUB, LHS, RHS,
                            attributes, reduce = FALSE)$closure

    LHS <- cbindSpM(LHS, A)
    RHS <- cbindSpM(RHS, B)

  }

  for (i in seq(n)) {

    A <- LHS %>% extract_columns(1)
    B <- RHS %>% extract_columns(1)

    LHS <- LHS %>% remove_columns(1)
    RHS <- RHS %>% remove_columns(1)

    A <- .compute_closure(A, LHS, RHS,
                          attributes, reduce = FALSE)$closure


    if (length(equalSpM(A, B)$pi) == 0) {

      LHS <- cbindSpM(LHS, A)
      RHS <- cbindSpM(RHS, B)

    }

  }

  return(list(lhs = LHS, rhs = .difference2(RHS, LHS)))

}

complete_rhs <- function(LHS, RHS) {

  n <- ncol.SpM(LHS)

  for (i in seq(n)) {

    A <- LHS %>% extract_columns(1)
    B <- RHS %>% extract_columns(1)

    LHS <- LHS %>% remove_columns(1)
    RHS <- RHS %>% remove_columns(1)

    AUB <- unionSpM(A, B)

    B <- .compute_closure(AUB, LHS, RHS,
                          attributes, reduce = FALSE)$closure

    LHS <- cbindSpM(LHS, A)
    RHS <- cbindSpM(RHS, B)

  }

  return(list(lhs = LHS, rhs = .difference2(RHS, LHS)))

}
