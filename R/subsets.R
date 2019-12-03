.subset <- function(x, y = NULL, proper = FALSE) {

  if (is.null(y)) y <- x

  stopifnot("x" %in% slotNames(x))

  stopifnot("x" %in% slotNames(y))

  p <- as.integer(rep(0, x@Dim[2] + 1))

  i <- is_subset_C(x@p, x@i, x@Dim, x@x,
                   y@p, y@i, y@Dim, y@x,
                   as.logical(proper), p)

  M <- build_sparse_matrix(i = i, p = p,
                           dims = c(y@Dim[2], x@Dim[2]))

  M <- t(M)

  empty <- which(colSums(x) == 0)

  if (length(empty) > 0) {

    M[empty, ] <- Matrix(TRUE, ncol = y@Dim[2], nrow = length(empty))

  }

  return(M)

}

#' @importFrom methods new
.equal_sets <- function(x, y = NULL, proper = FALSE) {

  if (is.null(y)) y <- x

  stopifnot("x" %in% slotNames(x))
  stopifnot("x" %in% slotNames(y))

  p <- as.integer(rep(0, x@Dim[2] + 1))
  i <- is_equal_set_C(x@p, x@i, x@Dim, x@x,
                      y@p, y@i, y@Dim, y@x,
                      as.logical(proper), p)

  t(new("ngCMatrix", p = p, i = i,
        Dim = c(y@Dim[2], x@Dim[2])))

}
