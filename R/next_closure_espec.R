.next_closure_sparse_espec <- function(B, i, imax,
                                       expanded_grades_set,
                                       LHS, RHS,
                                       attributes) {

  # i is the index of the last attribute

  count <- 0
  for (a_i in seq(i, 1)) {

    greater_grades <- expanded_grades_set[[a_i]]#[grades_set > B[a_i]]

    for (grade_i in greater_grades) {

      candidateBmax <- .direct_sum_sparse(B, a_i, grade_i, imax)

      # cat("Before SCT3\n")
      # cat(.sparse_set_to_string(candidateBmax, attributes), "\n")

      candidateBmax <- .compute_closure2(candidateBmax,
                                         LHS = LHS,
                                         RHS = RHS)

      # cat("After SCT3\n")
      # cat(.sparse_set_to_string(candidateBmax, attributes), "\n")
      count <- count + 1

      if (.is_set_preceding_i_j_sparse(B = B,
                                       C = candidateBmax,
                                       a_i = a_i,
                                       grade_i = grade_i)) {
        # cat("Used", count, "iterations in next_closure\n")

        return(candidateBmax)

      }

    }

  }

  return(Matrix(0, ncol = 1, nrow = imax, sparse = TRUE))

}
