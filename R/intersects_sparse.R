.intersects_sparse <- function(x, y = NULL, proper = FALSE) {

  if (is.null(y)) y <- x

  if ("x" %in% slotNames(x)) {

    my_x <- x@x

  } else {

    my_x <- rep(1, length(x@i))

  }

  if ("x" %in% slotNames(y)) {

    my_y <- y@x

  } else {

    my_y <- rep(1, length(y@i))

  }

  p <- as.integer(rep(0, x@Dim[2] + 1))
  i <- intersects_C(x@p, x@i, x@Dim, my_x,
                   y@p, y@i, y@Dim, my_y,
                   as.logical(proper), p)

  t(new("ngCMatrix", p = p, i = i,
        Dim = c(y@Dim[2], x@Dim[2])))

}
