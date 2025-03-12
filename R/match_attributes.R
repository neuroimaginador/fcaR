# Maps the origin Set to another Set with
# target_attributes, keeping the value of the attributes
# present in both attribute sets.
# @examples
# origin_attributes <- c(letters[1:4], "f")
# S$assign(b = 1, c = 0.5, f = 1)
# target_attributes <- c(letters[2:5], letters[1])
# R <- match_attributes(S, target_attributes)
match_attributes <- function(origin, target_attributes) {

  origin_attributes <- origin$get_attributes()

  idx <- match(origin_attributes, target_attributes)
  ii <- which(!is.na(idx))
  v <- origin$get_vector()

  target <- Set$new(attributes = target_attributes,
                    M = Matrix::sparseMatrix(
                      i = idx[ii],
                      j = rep(1, length(ii)),
                      x = v[ii],
                      dims = c(length(target_attributes), 1))
  )

  return(target)

}

match_implications <- function(origin, target_attributes) {

  origin_attributes <- origin$get_attributes()

  # if (all(origin_attributes == target_attributes))
  #   return(origin)

  idx <- match(origin_attributes, target_attributes)
  ii <- which(!is.na(idx))

  # cat("Matching:\n")
  # cat("\t", origin_attributes, "\n")
  # cat("to:\n")
  # cat("\t", target_attributes, "\n")
  # cat("Coinciding:\n")
  # cat("\t", idx[ii], "\n")

  newLHS <- Matrix::Matrix(0,
                           nrow = length(target_attributes),
                           ncol = origin$cardinality())
  rownames(newLHS) <- target_attributes

  for (i in ii) {

    newLHS[idx[i], ] <- origin$get_LHS_matrix()[i, ]

  }

  newRHS <- Matrix::Matrix(0,
                           nrow = length(target_attributes),
                           ncol = origin$cardinality())
  rownames(newRHS) <- target_attributes

  for (i in ii) {

    newRHS[idx[i], ] <- origin$get_RHS_matrix()[i, ]

  }

  ImplicationSet$new(
    attributes = target_attributes,
    lhs = newLHS, rhs = newRHS
  )

}
