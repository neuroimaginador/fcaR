.self_intersection <- function(x, y) {

  x <- methods::as(x, "dgCMatrix")
  y <- methods::as(y, "dgCMatrix")
  self_intersection_C(x_i = x@i,
                      x_p = x@p,
                      y_i = y@i,
                      y_p = y@p)

}
