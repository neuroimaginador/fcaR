#' @importFrom methods cbind2
.imp_to_basis_bg <- function(lhs_bg, rhs_bg, LHS, RHS, attributes) {

  n <- ncol.SpM(LHS)

  for (i in seq(n)) {

    # warning("LHS = ", LHS@Dim, inmediate.= TRUE, call. = FALSE)
    #
    A <- extract_columns(LHS, 1)
    B <- extract_columns(RHS, 1)

    LHS <- remove_columns(LHS, 1)
    RHS <- remove_columns(RHS, 1)

    # warning("A = ", A@Dim, inmediate.= TRUE, call. = FALSE)
    # warning("B = ", B@Dim, inmediate.= TRUE, call. = FALSE)

    AUB <- flattenSpM(cbindSpM(A, B))

    LHS_clos <- cbindSpM(lhs_bg, LHS)
    RHS_clos <- cbindSpM(rhs_bg, RHS)

    # warning("AUB = ", AUB@Dim, inmediate.= TRUE, call. = FALSE)
    # warning("LHS_clos = ", LHS_clos@Dim, inmediate.= TRUE, call. = FALSE)
    # warning("RHS_clos = ", RHS_clos@Dim, inmediate.= TRUE, call. = FALSE)
    #
    # c("ALL = ",
    #   AUB@Dim,
    #   LHS_clos@Dim,
    #   RHS_clos@Dim) %>%
    #   stringr::str_flatten(" ") %>%
    #   warning(inmediate. = TRUE, call. = FALSE)

    B <- .compute_closure(AUB, LHS_clos, RHS_clos,
                          attributes, reduce = TRUE)$closure

    LHS <- cbindSpM(LHS, A)
    RHS <- cbindSpM(RHS, B)

  }

  for (i in seq(n)) {

    A <- extract_columns(LHS, 1)
    B <- extract_columns(RHS, 1)

    LHS <- remove_columns(LHS, 1)
    RHS <- remove_columns(RHS, 1)

    LHS_clos <- cbindSpM(lhs_bg, LHS)
    RHS_clos <- cbindSpM(rhs_bg, RHS)

    # warning("AUB = ", AUB@Dim, inmediate.= TRUE, call. = FALSE)
    # warning("LHS_clos = ", LHS_clos@Dim, inmediate.= TRUE, call. = FALSE)
    # warning("RHS_clos = ", RHS_clos@Dim, inmediate.= TRUE, call. = FALSE)
    #
    # c("ALL = ",
    #   AUB@Dim,
    #   LHS_clos@Dim,
    #   RHS_clos@Dim) %>%
    #   stringr::str_flatten(" ") %>%
    #   warning(inmediate. = TRUE, call. = FALSE)

    A <- .compute_closure(A, LHS_clos, RHS_clos,
                          attributes, reduce = TRUE)$closure

    if (length(equalSpM(A, B)$i) == 0) {

      LHS <- cbindSpM(LHS, A)
      RHS <- cbindSpM(RHS, B)

    }

  }

  L <- list(lhs = LHS, rhs = RHS)
  L <- .Rsimplification_bg(lhs_bg = lhs_bg,
                           rhs_bg = rhs_bg,
                           LHS = L$lhs,
                           RHS = L$rhs)
  L <- .simplification_bg(lhs_bg = lhs_bg,
                          rhs_bg = rhs_bg,
                          lhs = L$lhs,
                          rhs = L$rhs)
  LHS <- L$lhs
  RHS <- L$rhs

  return(list(lhs = LHS, rhs = .difference2(RHS, LHS)))

}
