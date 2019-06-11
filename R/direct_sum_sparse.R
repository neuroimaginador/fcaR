.direct_sum_sparse <- function(B, attr_i, grade_i, I) {

  attributes <- colnames(I)

  a_i <- which(attributes == attr_i)

  the_sum <- B
  the_sum[a_i:length(attributes)] <- 0
  the_sum[a_i] <- grade_i

  return(the_sum)

}
