#' @import Matrix
convert_implication_list_to_sparse <- function(implication_list,
                                               attrs) {

  # Dimensions of the resulting matrices LHS and RHS
  n_implications <- length(implication_list)
  n_attributes <- length(attrs)

  # Compute LHS
  v <- lapply(seq(n_implications),
              function(i) fuzzy_set_to_sparse_coord(i,
                                                    set = implication_list[[i]]$get_lhs(),
                                                    attributes = attrs))

  m <- Reduce(rbind, v)

  LHS <- sparseMatrix(i = m[, "idx"],
                      j = m[, "i"],
                      x = m[, "values"],
                      dims = c(n_attributes,
                               n_implications))

  # RHS
  v <- lapply(seq(n_implications),
              function(i) fuzzy_set_to_sparse_coord(i,
                                                    set = implication_list[[i]]$get_rhs(),
                                                    attributes = attrs))

  m <- Reduce(rbind, v)

  RHS <- sparseMatrix(i = m[, "idx"],
                      j = m[, "i"],
                      x = m[, "values"],
                      dims = c(n_attributes,
                               n_implications))

  return(list(LHS = LHS, RHS = RHS))

}
