.reduce_transitivity <- function(M) {

  M <- to_matrix.SpM(M)
  D <- Matrix::Diagonal(n = ncol(M))
  adj <- M - D
  transitivity <- adj %*% adj
  transitivity@x[transitivity@x > 0] <- 1

  return(new_spm(adj - transitivity))

}

