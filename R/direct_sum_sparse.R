.direct_sum_sparse <- function(B, a_i, grade_i, imax) {

  the_sum <- B
  the_sum[a_i:imax] <- 0
  the_sum[a_i] <- grade_i

  return(the_sum)

}
