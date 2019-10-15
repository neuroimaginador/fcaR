.is_set_preceding <- function(B,
                                         C,
                                         a_i, grade_i) {

  ms_B <- B[a_i]

  ms_C <- C[a_i]

  if (ms_B >= ms_C) return(FALSE)

  if (!(ms_C == grade_i)) return(FALSE)

  if (a_i > 1) {

    # Check if B(k) == C(k) for k < a_i
    return(all(B[1:(a_i - 1)] == C[1:(a_i - 1)]))

  }

  return(TRUE)

}
