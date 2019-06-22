.next_closure_sparse <- function(B, I, grades_set,
                                 closure_function = .closure_sparse) {

  attributes <- colnames(I)

  for (attr_i in rev(attributes)) {

    greater_grades <- grades_set

    for (grade_i in greater_grades) {

      candidateBmax <- .direct_sum_sparse(B, attr_i, grade_i, I)

      candidateBmax <- closure_function(candidateBmax)

      if (.is_set_preceding_i_j_sparse(B = B,
                                       C = candidateBmax,
                                       attr_i = attr_i,
                                       grade_i = grade_i,
                                       I = I)) {

        return(candidateBmax)

      }

    }

  }

  return(Matrix(0, ncol = 1, nrow = length(attributes), sparse = TRUE))

}
