.self_intersection <- function(x, y) {

  x <- convert_to_sparse(x)
  y <- convert_to_sparse(y)
  self_intersection_C(x_i = x@i,
                      x_p = x@p,
                      y_i = y@i,
                      y_p = y@p)

}
