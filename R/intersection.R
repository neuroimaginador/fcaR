.intersection <- function(x, y, proper = FALSE) {

  p <- as.integer(rep(0, x@Dim[2] + 1))
  i <- intersects_C(x@p, x@i, x@Dim,
                    y@p, y@i, y@Dim, p)

  Matrix::t(Matrix::sparseMatrix(
    i = i, p = p, index1 = FALSE,
    dims = c(y@Dim[2], x@Dim[2])
  ))

}
