.process_batch <- function(LHS, RHS, attributes, rules, verbose = TRUE) {

  # Initialize results
  new_LHS <- Matrix::Matrix(0,
                            nrow = nrow(LHS),
                            ncol = 1,
                            sparse = TRUE)

  new_RHS <- Matrix::Matrix(0,
                            nrow = nrow(LHS),
                            ncol = 1,
                            sparse = TRUE)

  # Look up the equivalence rules in the registry
  methods <- lapply(rules,
                    equivalencesRegistry$get_entry)
  methods[sapply(methods, is.null)] <- NULL

  # Begin the timing
  tictoc::tic("batch")

  if (verbose) {

    message("Processing batch\n")#, i, "out of", length(idx) - 1, "\n")

  }

  old_LHS <- LHS
  old_RHS <- RHS
  new_cols <- ncol(LHS)

  # Loop over all functions
  for (j in seq_along(methods)) {

    current_cols <- new_cols

    current_rule <- methods[[j]]$fun

    tictoc::tic("rule")
    L <- current_rule(old_LHS, old_RHS, attributes)

    rule_time <- tictoc::toc(quiet = TRUE)
    old_LHS <- L$lhs
    old_RHS <- L$rhs

    new_cols <- ncol(old_LHS)

    if (verbose) {

      message("--> ", methods[[j]]$method[1], ": from ", current_cols, " to ",
              new_cols, " in ", round(rule_time$toc - rule_time$tic, 3),
              " secs.")

    }

  }

  # Add the computed implications to the set
  new_LHS <- cbind(new_LHS, old_LHS)
  new_RHS <- cbind(new_RHS, old_RHS)

  L <- .clean(new_LHS, new_RHS)

  batch_toc <- tictoc::toc(quiet = TRUE)

  if (verbose) {

    message("Batch took ", round(batch_toc$toc - batch_toc$tic, 3),
            " secs. \n")

  }

  return(list(lhs = L$lhs, rhs = L$rhs))

}
