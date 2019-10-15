.is_subset_sparse <- function(x, y = NULL, proper = FALSE) {

  if (is.null(y)) y <- x

  stopifnot("x" %in% slotNames(x))

  # if ("x" %in% slotNames(x)) {
  #
  #   my_x <- x@x
  #
  # } else {
  #
  #   my_x <- rep(1, length(x@i))
  #
  # }

  stopifnot("x" %in% slotNames(y))
  # if ("x" %in% slotNames(y)) {
  #
  #   my_y <- y@x
  #
  # } else {
  #
  #   my_y <- rep(1, length(y@i))
  #
  # }

  p <- as.integer(rep(0, x@Dim[2] + 1))

  i <- is_subset_C(x@p, x@i, x@Dim, x@x,
                   y@p, y@i, y@Dim, y@x,
                   as.logical(proper), p)

  M <- build_sparse_matrix(i = i, p = p, dims = c(y@Dim[2], x@Dim[2]))

  t(M)

}

.is_equal_set_sparse <- function(x, y = NULL, proper = FALSE) {

  if (is.null(y)) y <- x

  stopifnot("x" %in% slotNames(x))
  stopifnot("x" %in% slotNames(y))

  # if ("x" %in% slotNames(x)) {
  #
  #   my_x <- x@x
  #
  # } else {
  #
  #   my_x <- rep(1, length(x@i))
  #
  # }
  #
  # if ("x" %in% slotNames(y)) {
  #
  #   my_y <- y@x
  #
  # } else {
  #
  #   my_y <- rep(1, length(y@i))
  #
  # }

  p <- as.integer(rep(0, x@Dim[2] + 1))
  i <- is_equal_set_C(x@p, x@i, x@Dim, x@x,
                      y@p, y@i, y@Dim, y@x,
                      as.logical(proper), p)

  t(new("ngCMatrix", p = p, i = i,
        Dim = c(y@Dim[2], x@Dim[2])))

}

# .is_subset_binary <- function(x, y = NULL,
#                               proper = FALSE,
#                               transpose = FALSE) {
#
#   library(arules)
#
#   if (is.null(y)) y <- x
#
#   p <- as.integer(rep(0, x@Dim[2] + 1))
#
#   i <- .Call("R_is_subset",
#              x@p, x@i, x@Dim,
#              y@p, y@i, y@Dim,
#              as.logical(proper), p,
#              PACKAGE = "arules")
#
#   if (transpose) {
#
#     return(t(new("ngCMatrix", p = p, i = i,
#                  Dim = c(y@Dim[2], x@Dim[2]))))
#
#   } else {
#
#     return(new("ngCMatrix", p = p, i = i,
#                Dim = c(y@Dim[2], x@Dim[2])))
#
#   }
#
# }
