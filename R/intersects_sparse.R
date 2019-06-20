.intersects_sparse <- function(x, y = NULL, proper = FALSE) {

  if (is.null(y)) y <- x

  p <- as.integer(rep(0, x@Dim[2] + 1))
  i <- intersects_C(x@p, x@i, x@Dim,
                   y@p, y@i, y@Dim, p)

  t(new("ngCMatrix", p = p, i = i,
        Dim = c(y@Dim[2], x@Dim[2])))

}
