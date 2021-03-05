#' @importFrom methods cbind2
.imp_to_basis_bg <- function(lhs_bg, rhs_bg, LHS, RHS, attributes) {

  n <- ncol(LHS)

  for (i in seq(n)) {

    # warning("LHS = ", LHS@Dim, inmediate.= TRUE, call. = FALSE)
    #
    A <- .extract_column(LHS, 1)
    B <- .extract_column(RHS, 1)
    # A <- Matrix::Matrix(LHS[, 1], sparse = TRUE)
    #
    # B <- Matrix::Matrix(RHS[, 1], sparse = TRUE)

    LHS <- Matrix::Matrix(LHS[, -1], sparse = TRUE)
    RHS <- Matrix::Matrix(RHS[, -1], sparse = TRUE)

    # warning("A = ", A@Dim, inmediate.= TRUE, call. = FALSE)
    # warning("B = ", B@Dim, inmediate.= TRUE, call. = FALSE)

    # AUB <- .union(A, B)
    AUB <- .multiunion(cbind(A, B))

    LHS_clos <- methods::cbind2(lhs_bg, LHS)
    RHS_clos <- methods::cbind2(rhs_bg, RHS)

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

    LHS <- methods::cbind2(LHS, A)
    RHS <- methods::cbind2(RHS, B)

  }

  for (i in seq(n)) {

    A <- .extract_column(LHS, 1)
    B <- .extract_column(RHS, 1)
    # A <- Matrix::Matrix(LHS[, 1], sparse = TRUE)
    #
    # B <- Matrix::Matrix(RHS[, 1], sparse = TRUE)

    LHS <- Matrix::Matrix(LHS[, -1], sparse = TRUE)
    RHS <- Matrix::Matrix(RHS[, -1], sparse = TRUE)

    LHS_clos <- methods::cbind2(lhs_bg, LHS)
    RHS_clos <- methods::cbind2(rhs_bg, RHS)

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

    if (!(all(A == B))) {

      LHS <- methods::cbind2(LHS, A)
      RHS <- methods::cbind2(RHS, B)

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
