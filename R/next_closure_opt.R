.next_closure_opt <- function(B, i, imax, grades_set,
                                 closure_function = .closure_sparse) {

  # i is the index of the last attribute

  for (a_i in seq(i, 1)) {

    greater_grades <- grades_set[grades_set > B[a_i]]

    for (grade_i in greater_grades) {

      candidateBmax <- .direct_sum_sparse(B, a_i, grade_i, imax)

      candidateBmax <- closure_function(candidateBmax)

      difference <- candidateBmax
      difference[B >= difference] <- 0

      m <- min(c(difference@i + 1, imax))

      if (m >= a_i) {

        return(list(A = candidateBmax, i = a_i))

      }

    }

  }

  return(list(A = Matrix(0, ncol = 1, nrow = imax, sparse = TRUE),
              i = 1))

}
