#' @importFrom methods cbind2
.imp_to_basis_bg <- function(lhs_bg, rhs_bg, LHS, RHS, attributes) {

  # n <- ncol(LHS)

  # for (i in seq(n)) {
  #
  #   # warning("LHS = ", LHS@Dim, inmediate.= TRUE, call. = FALSE)
  #   #
  #   A <- Matrix::Matrix(LHS[, 1], sparse = TRUE)
  #
  #   B <- Matrix::Matrix(RHS[, 1], sparse = TRUE)
  #
  #   LHS <- Matrix::Matrix(LHS[, -1], sparse = TRUE)
  #   RHS <- Matrix::Matrix(RHS[, -1], sparse = TRUE)
  #
  #   AUB <- .union(A, B)
  #
  #   # warning("A = ", A@Dim, inmediate.= TRUE, call. = FALSE)
  #   # warning("B = ", B@Dim, inmediate.= TRUE, call. = FALSE)
  #
  #   # AUB <- .multiunion(cbind(A, B))
  #
  #   LHS_clos <- cbind(lhs_bg, LHS)
  #   RHS_clos <- cbind(rhs_bg, RHS)
  #
  #   # warning("AUB = ", AUB@Dim, inmediate.= TRUE, call. = FALSE)
  #   # warning("LHS_clos = ", LHS_clos@Dim, inmediate.= TRUE, call. = FALSE)
  #   # warning("RHS_clos = ", RHS_clos@Dim, inmediate.= TRUE, call. = FALSE)
  #   #
  #   # c("ALL = ",
  #   #   AUB@Dim,
  #   #   LHS_clos@Dim,
  #   #   RHS_clos@Dim) |>
  #   #   stringr::str_flatten(" ") |>
  #   #   warning(inmediate. = TRUE, call. = FALSE)
  #
  #   B <- .compute_closure(AUB, LHS_clos, RHS_clos,
  #                         attributes, reduce = FALSE)$closure
  #
  #   LHS <- cbind(LHS, A)
  #   RHS <- cbind(RHS, B)
  #
  # }
  #
  # for (i in seq(n)) {
  #
  #   A <- Matrix::Matrix(LHS[, 1], sparse = TRUE)
  #
  #   B <- Matrix::Matrix(RHS[, 1], sparse = TRUE)
  #
  #   LHS <- Matrix::Matrix(LHS[, -1], sparse = TRUE)
  #   RHS <- Matrix::Matrix(RHS[, -1], sparse = TRUE)
  #
  #   LHS_clos <- cbind(lhs_bg, LHS)
  #   RHS_clos <- cbind(rhs_bg, RHS)
  #
  #   # warning("AUB = ", AUB@Dim, inmediate.= TRUE, call. = FALSE)
  #   # warning("LHS_clos = ", LHS_clos@Dim, inmediate.= TRUE, call. = FALSE)
  #   # warning("RHS_clos = ", RHS_clos@Dim, inmediate.= TRUE, call. = FALSE)
  #   #
  #   # c("ALL = ",
  #   #   AUB@Dim,
  #   #   LHS_clos@Dim,
  #   #   RHS_clos@Dim) |>
  #   #   stringr::str_flatten(" ") |>
  #   #   warning(inmediate. = TRUE, call. = FALSE)
  #
  #   A <- .compute_closure(A, LHS_clos, RHS_clos,
  #                         attributes, reduce = FALSE)$closure
  #
  #   if (!(all(A == B))) {
  #
  #     LHS <- cbind(LHS, A)
  #     RHS <- cbind(RHS, B)
  #
  #   }
  #
  # }

  L <- list(lhs = LHS, rhs = RHS)
  L <- .Rsimplification_bg(
    LHS = cbind(lhs_bg, L$lhs),
    RHS = cbind(rhs_bg, L$rhs),
    fixed = ncol(lhs_bg))
  L <- .simplification_bg(
    LHS = cbind(lhs_bg, L$lhs),
    RHS = cbind(rhs_bg, L$rhs),
    fixed = ncol(lhs_bg))
  LHS <- L$lhs
  RHS <- L$rhs

  return(list(lhs = LHS, rhs = .difference2(RHS, LHS)))

}
