sparse_set_difference <- function(A, B) {

  if (is.numeric(A)) A <- Matrix(A, sparse = TRUE)
  if (is.numeric(B)) B <- Matrix(B, sparse = TRUE)

  if (ncol(A) == ncol(B)) {

    A[B >= A] <- 0

    return(A)

  }

  if (ncol(B) == 1) {

    n <- ncol(A)

    new_i <- rep(B@i, n)

    if ("x" %in% slotNames(B)) {

      new_x <- rep(B@x, n)

    } else {

      new_x <- rep(TRUE, length(new_i))

    }

    new_p <- c(0, B@p[2] * seq(n))

    newB <- sparseMatrix(i = new_i + 1,
                         p = new_p,
                         x = new_x,
                         dims = c(nrow(B), n))

    A[newB >= A] <- 0

    return(A)

  }

  if (ncol(A) == 1) {

    n <- ncol(B)

    new_i <- rep(A@i, n)

    if ("x" %in% slotNames(A)) {

      new_x <- rep(A@x, n)

    } else {

      new_x <- rep(TRUE, length(new_i))

    }

    new_p <- c(0, A@p[2] * seq(n))

    newA <- sparseMatrix(i = new_i + 1,
                         p = new_p,
                         x = new_x,
                         dims = c(nrow(A), n))

    newA[B >= newA] <- 0

    return(newA)

  }

  stop("Case not implemented yet.")

}
