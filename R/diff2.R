.difference2 <- function(A, B) {

  # if (is.numeric(A)) A <- Matrix::Matrix(A, sparse = TRUE)
  # if (is.numeric(B)) B <- Matrix::Matrix(B, sparse = TRUE)

  applicable <- (ncol.SpM(A) == ncol.SpM(B)) ||
    (ncol.SpM(B) == 1) || (ncol.SpM(A) == 1)
  stopifnot(applicable)

  if (ncol.SpM(A) == ncol.SpM(B)) {

    A <- set_difference_SpM(A$pi - 1, A$pp, A$px,
                            B$pi - 1, B$pp, B$px,
                            nrow.SpM(A))

    return(A)

  }

  if (ncol.SpM(B) == 1) {

    n <- ncol.SpM(A)

    L <- set_difference_single_SpM(A$pi - 1, A$pp, A$px,
                                   B$pi - 1, B$pp, B$px,
                                   nrow.SpM(A))

    return(L)

  }

  if (ncol.SpM(A) == 1) {

    n <- ncol.SpM(B)

    newA <- A %>% replicate(n)

    newA <- set_difference_SpM(newA$pi - 1, newA$pp, newA$px,
                               B$pi - 1, B$pp, B$px,
                               nrow.SpM(newA))

    return(newA)

  }

}
