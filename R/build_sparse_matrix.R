build_sparse_matrix <- function(i, p, x = NULL, dims) {

  if (is.null(x)) {
    # Boolean (pattern) matrix — no x slot
    res <- Matrix::sparseMatrix(
      i = i, p = p, index1 = FALSE,
      dims = dims
    )
  } else {
    res <- Matrix::sparseMatrix(
      i = i, p = p, x = x, index1 = FALSE,
      dims = dims
    )
  }

  return(res)

}
