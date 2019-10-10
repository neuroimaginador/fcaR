#' @import sets
.compute_next_intent_old <- function(B, I, grades_set) {

  attributes <- colnames(I)
  has_maximum_i_j <- FALSE
  imax <- NA
  grademax <- NA
  Bmax <- NA

  ms_attributes <- .get_attribute_memberships(B, I)

  for (attr_i in attributes) {

    # my_grade_i <- ms_attributes[attr_i]
    # greater_grades <- grades_set[grades_set >= my_grade_i]
    greater_grades <- grades_set

    for (grade_i in greater_grades) {

      # cat("i = ", imax, " to i = ", attr_i, "\n")
      # cat("j = ", grademax, " to j = ", grade_i, "\n")

      candidateBmax <- .direct_sum(B, attr_i, grade_i, I)
      print(B)
      print(candidateBmax)

      print(.is_set_preceding_i_j(B = B,
                            C = candidateBmax,
                            attr_i = attr_i,
                            grade_i = grade_i,
                            I = I))

      if (!has_maximum_i_j) {

        if (.is_set_preceding_i_j(B = B,
                                  C = candidateBmax,
                                  attr_i = attr_i,
                                  grade_i = grade_i,
                                  I = I)) {
          print("Better")
          Bmax <- candidateBmax
          imax <- attr_i
          grademax <- grade_i

          has_maximum_i_j <- TRUE

        }

      } else {

        cat("(i,j) = (", imax, ", ", grademax,
            ") - (r,s) = (", attr_i, ", ", grade_i, ")\n")

        print(.is_lower_than(imax, grademax,
                             attr_i, grade_i, I = I))
        if (.is_set_preceding_i_j(B = B,
                                  C = candidateBmax,
                                  attr_i = attr_i,
                                  grade_i = grade_i,
                                  I = I) &&
            .is_lower_than(imax, grademax,
                           attr_i, grade_i, I = I)) {

          print("Better2")

          Bmax <- candidateBmax
          imax <- attr_i
          grademax <- grade_i

        }

      }

    }

  }

  return(Bmax)

}
