#' @import sets
.compute_next_intent <- function(B, I, grades_set) {

  attributes <- colnames(I)

  for (attr_i in rev(attributes)) {

    greater_grades <- grades_set

    for (grade_i in greater_grades) {

      candidateBmax <- .direct_sum(B, attr_i, grade_i, I)

      if (.is_set_preceding_i_j(B = B,
                                C = candidateBmax,
                                attr_i = attr_i,
                                grade_i = grade_i,
                                I = I)) {

        return(candidateBmax)

      }

    }

  }

  return(gset())

}
