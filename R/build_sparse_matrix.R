build_sparse_matrix <- function(i, p, x = NULL, dims) {

  res <- structure(.Data = list(), i = i, p = p, x = x,
                   Dim = dims, Dimnames = vector(mode = "list",
                                                 length = length(dims)),
                   factors = list(),
                   class = ifelse(is.null(x), "ngCMatrix", "dgCMatrix"))

  return(res)

}
