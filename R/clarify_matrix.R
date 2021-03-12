.clarify_matrix <- function(I, rows, cols) {

  identical <- equalSpM(I)

  equal_attributes <- which(colSums(identical) > 1)
  independent_att <- which(colSums(identical) == 1)

  new_att <- c(cols[independent_att])
  keep <- c(independent_att)

  while (length(equal_attributes) > 0) {

    j <- equal_attributes[1]
    keep <- c(keep, j)

    # v <- identical %>% extract_columns(j)
    i <- (identical %>% extract_columns(j))$pi
    new_att <- c(new_att,
                 paste0("[",
                        stringr::str_flatten(cols[i],
                                    collapse = ", "),
                        "]"))
    equal_attributes <- setdiff(equal_attributes, i)

  }

  my_I <- I
  my_I <- my_I %>% extract_columns(keep)
  assign_colnamesSpM(my_I, new_att)
  assign_rownamesSpM(my_I, rows)

  return(my_I)

}
