.extract_column <- function(M, j) {

  i <- M@i + 1
  ends <- c(M@p[j] + 1, M@p[j + 1])

  if (ends[1] > ends[2]) {

    return(Matrix(0,
                  nrow = M@Dim[1],
                  ncol = 1,
                  sparse = TRUE))

  }

  idx <- seq(ends[1], ends[2])
  x <- M@x

  if (length(idx) > 0) {

    sparseMatrix(i = i[idx],
                 p = c(0, length(idx)),
                 x = x[idx],
                 dims = c(M@Dim[1], 1))

  } else {

    Matrix(0,
           nrow = M@Dim[1],
           ncol = 1,
           sparse = TRUE)

  }

}
