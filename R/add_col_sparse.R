add_col <- function(M, sparse_v) {

  newM <- M

  newM@i <- c(newM@i, as.integer(sparse_v@i))
  newM@p <- c(newM@p, max(newM@p) + length(sparse_v@i))
  newM@Dim[2] <- as.integer(newM@Dim[2] + 1)
  newM@x <- c(newM@x, sparse_v@x)

  return(newM)

}

add_cols <- function(M, cols) {

  newM <- M

  newM@i <- c(newM@i, as.integer(cols@i))
  newM@p <- c(newM@p, max(newM@p) + cols@p[-1])
  newM@Dim[2] <- as.integer(newM@Dim[2] + cols@Dim[2])
  newM@x <- c(newM@x, cols@x)

  return(newM)

}
