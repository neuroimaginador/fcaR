.is_set_preceding_i_j <- function(B, C,
                                  attr_i, grade_i,
                                  I) {
  attributes <- colnames(I)
  a_i <- which(attributes == attr_i)

  ms_B <- .get_attribute_memberships(B, I)[attr_i]

  ms_C <- .get_attribute_memberships(C, I)[attr_i]

  if (ms_B >= ms_C) return(FALSE)

  if (!(ms_C == grade_i)) return(FALSE)

  if (a_i > 1) {

    # Check if B(k) == C(k) for k < a_i
    lower_attr <- attributes[1:(a_i - 1)]
    ms_B <- .get_attribute_memberships(B, I)[lower_attr]
    ms_C <- .get_attribute_memberships(C, I)[lower_attr]

    return(all(ms_B == ms_C))

  }

  return(TRUE)

}
