#' @description
#' Scale a fuzzy context to a binary one for concept extraction.
#' @noRd
internal_scale_fuzzy <- function(fc) {
  L <- sort(fc$grades_set)
  I <- as.matrix(fc$incidence())
  n_obj <- nrow(I)
  n_att <- ncol(I)
  n_grades <- length(L)
  
  # New dimensions: (n_obj * n_grades) x (n_att * n_grades)
  # Relation: min(alpha, beta) <= I(g, m)
  sc_I <- matrix(FALSE, nrow = n_obj * n_grades, ncol = n_att * n_grades)
  
  for (i in 1:n_obj) {
    for (j in 1:n_att) {
      val <- I[i, j]
      for (idx_a in 1:n_grades) {
        for (idx_b in 1:n_grades) {
          if (min(L[idx_a], L[idx_b]) <= val) {
            sc_I[(i-1)*n_grades + idx_a, (j-1)*n_grades + idx_b] <- TRUE
          }
        }
      }
    }
  }
  
  res <- FormalContext$new(sc_I)
  return(res)
}

#' @description
#' Find fuzzy concepts using Scaling + InClose.
#' 
#' @param fc A FormalContext object.
#' @param verbose (logical) If TRUE, print progress messages.
#' @return A list of fuzzy concepts, each with an extent and intent.
#' @noRd
internal_find_fuzzy_concepts <- function(fc, verbose = FALSE) {
  if (verbose) message("Scaling fuzzy context...")
  sc <- internal_scale_fuzzy(fc)
  
  if (verbose) message("Running InClose on scaled context...")
  sc$find_concepts()
  
  # Reverse mapping
  L <- sort(fc$grades_set)
  n_grades <- length(L)
  orig_objects <- fc$objects
  orig_attributes <- fc$attributes
  
  extents_mat <- sc$concepts$extents()
  intents_mat <- sc$concepts$intents()
  n_concepts <- ncol(extents_mat)
  
  results <- list()
  for (i in seq_len(n_concepts)) {
    ext_idx <- which(extents_mat[, i] > 0)
    int_idx <- which(intents_mat[, i] > 0)
    
    A_fuzzy <- setNames(numeric(length(orig_objects)), orig_objects)
    for (idx in ext_idx) {
       obj_idx <- ceiling(idx / n_grades)
       grade_idx <- (idx - 1) %% n_grades + 1
       A_fuzzy[obj_idx] <- max(A_fuzzy[obj_idx], L[grade_idx])
    }
    
    B_fuzzy <- setNames(numeric(length(orig_attributes)), orig_attributes)
    for (idx in int_idx) {
       att_idx <- ceiling(idx / n_grades)
       grade_idx <- (idx - 1) %% n_grades + 1
       B_fuzzy[att_idx] <- max(B_fuzzy[att_idx], L[grade_idx])
    }
    
    results[[i]] <- list(extent = A_fuzzy, intent = B_fuzzy)
  }
  
  return(results)
}
