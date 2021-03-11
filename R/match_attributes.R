# Maps the origin SparseSet to another SparseSet with
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

  M <- matrix(0, nrow = length(target_attributes), ncol = 1)
  M[idx[ii]] <- (v %>% to_matrix.SpM())[, ii]

  target <- SparseSet$new(attributes = target_attributes,
                          M = new_spm(M))

  return(target)

}
