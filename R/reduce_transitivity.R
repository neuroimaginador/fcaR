.reduce_transitivity_legacy <- function(M) {

  D <- Matrix::Diagonal(n = ncol(M))
  adj <- M - D
  transitivity <- adj %*% adj
  transitivity@x[transitivity@x > 0] <- 1

  return(adj - transitivity)

}

# Wrapper en R
#' @importFrom methods new
.reduce_transitivity <- function(M) {
  # Asegurar dgCMatrix
  if (!inherits(M, "nMatrix")) M <- methods::as(M, "nMatrix")

  # Llamada C++
  L <- reduce_transitivity_cpp(M@i, M@p, M@Dim)

  # Reconstruir objeto
  new("ngCMatrix",
      i = L$i,
      p = L$p,
      Dim = L$Dim)
}
