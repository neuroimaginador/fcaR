.intersection <- function(x, y, proper = FALSE) {

  p <- as.integer(rep(0, dim.SpM(x)[2] + 1))
  i <- intersects_C(x$pp, x$pi, dim.SpM(x),
                    y$pp, y$pi, dim.SpM(y), p)

  M <- new_spm(i = i, p = p, nrow = dim.SpM(y)[2])

  return(M)

}
