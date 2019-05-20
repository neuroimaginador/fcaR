
# Total order in Y x L (attributes x truth values)
.is_lower_than <- function(attr_i, grade_i, attr_j, grade_j, I) {

  # (a_i, g_i) <= (a_j, g_j) <=> a_i < a_j or a_i == a_j and g_i >= g_j

  attributes <- colnames(I)

  a_i <- which(attributes == attr_i)
  a_j <- which(attributes == attr_i)

  if (a_i > a_j) {

    return(FALSE)

  } else {

    if (a_i < a_j) {

      return(TRUE)

    } else {

      return(grade_i >= grade_j)

    }

  }

}
