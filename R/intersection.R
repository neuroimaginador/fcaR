.intersection <- function(x, y, proper = FALSE) {

  p <- as.integer(rep(0, x@Dim[2] + 1))
  i <- intersects_C(x@p, x@i, x@Dim,
                    y@p, y@i, y@Dim, p)

  M <- build_sparse_matrix(i = i, p = p,
                           dims = c(y@Dim[2], x@Dim[2]))

  return(M)

  # M <- Matrix::t(M)
  #
  #
  # Matrix::t(methods::new("ngCMatrix", p = p, i = i,
  #                        Dim = c(y@Dim[2], x@Dim[2])))

}
