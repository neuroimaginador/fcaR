add_col <- function(M, sparse_v) {

  sparse_v <- convert_to_sparse(sparse_v)

  newM <- M

  newM@i <- c(newM@i, as.integer(sparse_v@i))
  newM@p <- c(newM@p, max(newM@p) + length(sparse_v@i))
  newM@Dim[2] <- as.integer(newM@Dim[2] + 1)

  if (!methods::is(M, "ngCMatrix")) {

    newM@x <- c(newM@x, sparse_v@x)

  }

  return(newM)

}
