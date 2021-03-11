.reduce_transitivity <- function(M) {

  # TODO: Adapt to SpM
  D <- Matrix::Diagonal(n = ncol(M))
  adj <- M - D
  transitivity <- adj %*% adj
  transitivity@x[transitivity@x > 0] <- 1

  return(adj - transitivity)

}

