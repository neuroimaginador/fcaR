#' @import sets
.compute_next_intent <- function(B, I, grades_set) {

  attributes <- colnames(I)
  has_maximum_i_j <- FALSE
  imax <- NA
  grademax <- NA
  Bmax <- NA

  ms_attributes <- .get_attribute_memberships(B, I)

  for (attr_i in attributes) {

    my_grade_i <- ms_attributes[attr_i]
    greater_grades <- grades_set[grades_set >= my_grade_i]
    # greater_grades <- grades_set

    for (grade_i in greater_grades) {

      if (!has_maximum_i_j) {

        candidateBmax <- .direct_sum(B, attr_i, grade_i, I)

        if (.is_set_preceding_i_j(B = B,
                                 C = candidateBmax,
                                 attr_i = attr_i,
                                 grade_i = grade_i,
                                 I = I)) {

          Bmax <- candidateBmax
          imax <- attr_i
          grademax <- grade_i

          has_maximum_i_j <- TRUE

        }

      } else {

        candidateBmax <- .direct_sum(B, attr_i, grade_i, I)

        if (.is_set_preceding_i_j(B = B,
                                 C = candidateBmax,
                                 attr_i = attr_i,
                                 grade_i = grade_i,
                                 I = I) &&
            .is_lower_than(imax, grademax,
                          attr_i, grade_i, I = I)) {

          Bmax <- candidateBmax
          imax <- attr_i
          grademax <- grade_i

        }

      }

    }

  }

  return(Bmax)

}
