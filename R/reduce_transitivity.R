.reduce_transitivity <- function(M) {

  D <- Matrix::Diagonal(n = ncol(M))
  adj <- M - D
  transitivity <- adj %*% adj
  transitivity@x[transitivity@x > 0] <- 1

  return(adj - transitivity)

}

