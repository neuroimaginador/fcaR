sparse_set_difference <- function(A, B) {

  if (is.numeric(A)) A <- Matrix(A, sparse = TRUE)
  if (is.numeric(B)) B <- Matrix(B, sparse = TRUE)

  if (ncol(A) == ncol(B)) {

    A[B >= A] <- 0

    return(A)

  }

  if (ncol(B) == 1) {

    n <- ncol(A)

    newB <- replicate_sparse_col(B, n)

    A[newB >= A] <- 0

    return(A)

  }

  if (ncol(A) == 1) {

    n <- ncol(B)

    newA <- replicate_sparse_col(A, n)

    newA[B >= newA] <- 0

    return(newA)

  }

  stop("Case not implemented yet.")

}

sparse_set_union <- function(A, B) {

  if (is.numeric(A)) A <- Matrix(A, sparse = TRUE)
  if (is.numeric(B)) B <- Matrix(B, sparse = TRUE)

  if (ncol(A) == ncol(B)) {

    A[B > A] <- B[B > A]

    return(A)

  }

  if (ncol(B) == 1) {

    n <- ncol(A)

    newB <- replicate_sparse_col(B, n)

    A[newB > A] <- newB[newB > A]

    return(A)

  }

  if (ncol(A) == 1) {

    n <- ncol(B)

    newA <- replicate_sparse_col(A, n)

    newA[B > newA] <- B[B > newA]

    return(newA)

  }

  stop("Case not implemented yet.")

}



replicate_sparse_col <- function(A, n) {

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

  return(newA)

}
