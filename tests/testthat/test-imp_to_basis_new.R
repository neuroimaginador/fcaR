test_that("imp_to_basis C++ implementation matches R logic", {
    # Original R implementation
    imp_to_basis_old <- function(LHS, RHS, attributes = NULL) {
        n <- ncol(LHS)

        # Helper to access internal functions
        .union <- fcaR:::.union
        .compute_closure <- fcaR:::.compute_closure
        .difference2 <- fcaR:::.difference2

        for (i in seq(n)) {
            A <- Matrix::Matrix(LHS[, 1], sparse = TRUE)
            B <- Matrix::Matrix(RHS[, 1], sparse = TRUE)

            LHS <- Matrix::Matrix(LHS[, -1], sparse = TRUE)
            RHS <- Matrix::Matrix(RHS[, -1], sparse = TRUE)

            AUB <- .union(A, B)

            B <- .compute_closure(
                AUB,
                LHS,
                RHS,
                attributes,
                reduce = FALSE
            )$closure

            LHS <- cbind(LHS, A)
            RHS <- cbind(RHS, B)
        }

        for (i in seq(n)) {
            A <- Matrix::Matrix(LHS[, 1], sparse = TRUE)
            B <- Matrix::Matrix(RHS[, 1], sparse = TRUE)

            LHS <- Matrix::Matrix(LHS[, -1], sparse = TRUE)
            RHS <- Matrix::Matrix(RHS[, -1], sparse = TRUE)

            A <- .compute_closure(
                A,
                LHS,
                RHS,
                attributes,
                reduce = FALSE
            )$closure

            if (!(all(A == B))) {
                LHS <- cbind(LHS, A)
                RHS <- cbind(RHS, B)
            }
        }

        res <- list(lhs = LHS, rhs = .difference2(RHS, LHS))
        if (!is.null(attributes)) {
            rownames(res$lhs) <- attributes
            rownames(res$rhs) <- attributes
        }
        return(res)
    }

    # Generate random sparse matrices
    set.seed(123)
    n_attr <- 10
    n_imps <- 20

    # Create indices for sparse matrix
    i_lhs <- sample(0:(n_attr - 1), 30, replace = TRUE)
    j_lhs <- sample(0:(n_imps - 1), 30, replace = TRUE)
    x_lhs <- runif(30)

    i_rhs <- sample(0:(n_attr - 1), 30, replace = TRUE)
    j_rhs <- sample(0:(n_imps - 1), 30, replace = TRUE)
    x_rhs <- runif(30)

    LHS <- Matrix::sparseMatrix(
        i = i_lhs + 1,
        j = j_lhs + 1,
        x = x_lhs,
        dims = c(n_attr, n_imps)
    )
    RHS <- Matrix::sparseMatrix(
        i = i_rhs + 1,
        j = j_rhs + 1,
        x = x_rhs,
        dims = c(n_attr, n_imps)
    )

    # Run both
    res_old <- imp_to_basis_old(LHS, RHS)

    # New implementation is now fcaR:::.imp_to_basis
    res_new <- fcaR:::.imp_to_basis(LHS, RHS, NULL)

    # Compare
    # Order might differ?
    # Ideally the algorithm is deterministic and order preserving for the queue.
    # But removing elements in second pass might affect order if implementation details differ slightly.
    # However, simulating the exact loop structure should preserve order.

    expect_equal(dim(res_old$lhs), dim(res_new$lhs))
    expect_equal(dim(res_old$rhs), dim(res_new$rhs))

    # Check content
    # Convert to dense for easier comparison of values
    expect_equal(
        as.matrix(res_old$lhs),
        as.matrix(res_new$lhs),
        tolerance = 1e-6
    )
    expect_equal(
        as.matrix(res_old$rhs),
        as.matrix(res_new$rhs),
        tolerance = 1e-6
    )
})
