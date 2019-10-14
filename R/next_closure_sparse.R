.next_closure_sparse <- function(B, i, imax, grades_set,
                                 closure_function = .closure_sparse) {

  # i is the index of the last attribute

  for (a_i in seq(i, 1)) {

    greater_grades <- grades_set#[grades_set > B[a_i]]

    for (grade_i in greater_grades) {

      candidateBmax <- .direct_sum_sparse(B, a_i, grade_i, imax)

      candidateBmax <- closure_function(candidateBmax)

      if (.is_set_preceding_i_j_sparse(B = B,
                                       C = candidateBmax,
                                       a_i = a_i,
                                       grade_i = grade_i)) {

        return(candidateBmax)

      }

    }

  }

  # Never get to this line
  # return(Matrix(0, ncol = 1, nrow = imax, sparse = TRUE))

}
