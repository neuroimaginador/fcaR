.intersection <- function(x, y, proper = FALSE) {

  p <- as.integer(rep(0, dim.SpM(x)[2] + 1))
  i <- intersects_C(as.integer(x$pp), as.integer(x$pi - 1), as.integer(dim.SpM(x)),
                    as.integer(y$pp), as.integer(y$pi - 1), as.integer(dim.SpM(y)),
                    p)

  M <- new_spm(i = i, p = p, nrow = dim.SpM(y)[2])

  return(M)

}
