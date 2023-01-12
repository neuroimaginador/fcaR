.clarify_matrix <- function(I, rows, cols) {

  identical <- .equal_sets(I)

  equal_attributes <- which(Matrix::colSums(identical) > 1)
  independent_att <- which(Matrix::colSums(identical) == 1)

  new_att <- c(cols[independent_att])
  keep <- c(independent_att)

  while (length(equal_attributes) > 0) {

    j <- equal_attributes[1]
    keep <- c(keep, j)

    v <- identical[, j]
    i <- which(v > 0)
    new_att <- c(new_att,
                 paste0("[",
                        stringr::str_flatten(cols[i],
                                    collapse = ", "),
                        "]"))
    equal_attributes <- setdiff(equal_attributes, i)

  }

  my_I <- I
  if (length(keep) == 1) {

    my_I <- .extract_column(my_I, keep)

  } else {

    my_I <- my_I[, keep]

  }

  if (is.vector(my_I)) {

    my_I <- Matrix::Matrix(my_I, nrow = 1, ncol = length(keep), sparse = TRUE)

  }

  colnames(my_I) <- new_att
  rownames(my_I) <- rows

  return(my_I)

}
