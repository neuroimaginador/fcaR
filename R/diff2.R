.difference2 <- function(A, B) {

  if (is.numeric(A)) A <- Matrix::Matrix(A, sparse = TRUE)
  if (is.numeric(B)) B <- Matrix::Matrix(B, sparse = TRUE)

  applicable <- (ncol(A) == ncol(B)) || (ncol(B) == 1) || (ncol(A) == 1)
  stopifnot(applicable)

  if (ncol(A) == ncol(B)) {

    A <- set_difference(A@i, A@p, A@x, B@i, B@p, B@x, nrow(A))
    # A[B >= A] <- 0

    return(A)

  }

  if (ncol(B) == 1) {

    n <- ncol(A)

    # newB <- .replicate_col(B, n)

    A <- set_difference_single(A@i, A@p, A@x, B@i, B@p, B@x, nrow(A))

    # A[newB >= A] <- 0

    return(A)

  }

  if (ncol(A) == 1) {

    n <- ncol(B)

    newA <- .replicate_col(A, n)

    newA <- set_difference(newA@i, newA@p, newA@x, B@i, B@p, B@x, nrow(newA))

    # newA[B >= newA] <- 0

    return(newA)

  }

}
