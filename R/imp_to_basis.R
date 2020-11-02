.imp_to_basis <- function(LHS, RHS, attributes) {

  n <- ncol(LHS)

  for (i in seq(n)) {

    # cat("=========\n")
    #
    # print(i)

    A <- Matrix(LHS[, 1], sparse = TRUE)

    # cat("A = ")
    # print(SparseSet$new(attributes = attributes,
    #                     M = A))
    # cat("\n")

    B <- Matrix(RHS[, 1], sparse = TRUE)

    # cat("B = ")
    # print(SparseSet$new(attributes = attributes,
    #                     M = B))
    # cat("\n")

    LHS <- Matrix(LHS[, -1], sparse = TRUE)
    RHS <- Matrix(RHS[, -1], sparse = TRUE)

    # cat("Implications = \n")
    # foo_imp <- ImplicationSet$new(attributes = attributes,
    #                               lhs = LHS,
    #                               rhs = RHS)
    # print(foo_imp)

    AUB <- .union(A, B)

    # cat("AUB = ")
    # print(SparseSet$new(attributes = attributes,
    #                     M = AUB))
    # cat("\n")

    B <- .compute_closure(AUB, LHS, RHS,
                          attributes, reduce = FALSE)$closure

    # cat("newB = ")
    # print(SparseSet$new(attributes = attributes,
    #                     M = B))
    # cat("\n")
    LHS <- cbind(LHS, A)
    RHS <- cbind(RHS, B)

  }

  for (i in seq(n)) {

    # cat("==========\n")
    # print(i)

    A <- Matrix(LHS[, 1], sparse = TRUE)

    # cat("A = ")
    # print(SparseSet$new(attributes = attributes,
    #                     M = A))
    # cat("\n")

    B <- Matrix(RHS[, 1], sparse = TRUE)

    # cat("B = ")
    # print(SparseSet$new(attributes = attributes,
    #                     M = B))
    # cat("\n")

    LHS <- Matrix(LHS[, -1], sparse = TRUE)
    RHS <- Matrix(RHS[, -1], sparse = TRUE)

    # cat("Implications = \n")
    # foo_imp <- ImplicationSet$new(attributes = attributes,
    #                               lhs = LHS,
    #                               rhs = RHS)
    # print(foo_imp)


    A <- .compute_closure(A, LHS, RHS,
                          attributes, reduce = FALSE)$closure

    # cat("newA = ")
    # print(SparseSet$new(attributes = attributes,
    #                     M = A))
    # cat("\n")

    if (!(all(A == B))) {

      # cat("Added: ")
      # print(SparseSet$new(attributes = attributes,
      #                     M = A))
      # cat(" -> ")
      # print(SparseSet$new(attributes = attributes,
      #                     M = B))
      # cat("\n")


      LHS <- cbind(LHS, A)
      RHS <- cbind(RHS, B)

    }

  }

  return(list(lhs = LHS, rhs = .difference2(RHS, LHS)))

}
