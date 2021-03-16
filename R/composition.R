.composition <- function(LHS, RHS, attributes) {

  # logic_name <- tolower(fuzzy_logic()$name)

  equal_LHS <- equalSpM(LHS)

  replicas <- which(colSums(equal_LHS) > 1)

  marked_to_remove <- rep(FALSE, ncol.SpM(LHS))

  if (length(replicas) > 0) {

    for (rep_id in seq_along(replicas)) {

      if (marked_to_remove[replicas[rep_id]]) next

      ids_to_merge <- (equal_LHS %>%
        extract_columns(replicas[rep_id]))$pi

      B <- RHS %>%
        extract_columns(ids_to_merge) %>%
        flattenSpM()

      RHS <- cbindSpM(RHS, B)
      LHS <- cbindSpM(LHS,
                      extract_columns(LHS,
                                      ids_to_merge[1]))

      marked_to_remove[ids_to_merge] <- TRUE

    }

  }

  remove_id <- which(marked_to_remove)

  if (length(remove_id) > 0) {

    LHS <- LHS %>% remove_columns(remove_id)
    RHS <- RHS %>% remove_columns(remove_id)

  }

  return(list(lhs = LHS,
              rhs = RHS))

}
