.reduce_transitivity <- function(M) {

  adj <- matrix(0, nrow = nrow(M), ncol = ncol(M))
  adj[which(as.vector(!(M == 0)))] <- 1
  adj <- adj - diag(nrow = nrow(M), ncol = ncol(M))

  transitivity <- ifelse(adj %*% adj > 0, 1, 0)
  return(adj - transitivity)

}

