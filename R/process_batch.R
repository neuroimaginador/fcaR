.process_batch <- function(LHS, RHS, attributes, rules, verbose = TRUE) {

  if (requireNamespace("tictoc", quietly = TRUE)) {

    timing <- TRUE

  } else {

    timing <- FALSE

  }

  # Initialize results
  new_LHS <- NULL#zeroSpM(nrow = nrow.SpM(LHS), ncol = 1)

  new_RHS <- NULL#zeroSpM(nrow = nrow.SpM(LHS), ncol = 1)

  # Look up the equivalence rules in the registry
  methods <- lapply(rules,
                    equivalencesRegistry$get_entry)
  methods[sapply(methods, is.null)] <- NULL

  # Begin the timing
  if (timing) tictoc::tic("batch")

  if (verbose) {

    message("Processing batch\n")#, i, "out of", length(idx) - 1, "\n")

  }

  old_LHS <- LHS
  old_RHS <- RHS
  new_cols <- ncol.SpM(LHS)

  # Loop over all functions
  for (j in seq_along(methods)) {

    current_cols <- new_cols

    current_rule <- methods[[j]]$fun

    if (timing) tictoc::tic("rule")
    L <- current_rule(old_LHS, old_RHS, attributes)

    if (timing) rule_time <- tictoc::toc(quiet = TRUE)
    old_LHS <- L$lhs
    old_RHS <- L$rhs

    new_cols <- ncol.SpM(old_LHS)

    if (verbose) {

      if (timing) {

        message("--> ", methods[[j]]$method[1], ": from ", current_cols, " to ",
                new_cols, " in ", round(rule_time$toc - rule_time$tic, 3),
                " secs.")

      } else {

        message("--> ", methods[[j]]$method[1], ": from ", current_cols, " to ",
                new_cols, ".")

      }


    }

  }

  # Add the computed implications to the set
  new_LHS <- cbindSpM(new_LHS, old_LHS)
  new_RHS <- cbindSpM(new_RHS, old_RHS)

  L <- .clean(new_LHS, new_RHS)

  if (timing) batch_toc <- tictoc::toc(quiet = TRUE)

  if (verbose && timing) {

    message("Batch took ", round(batch_toc$toc - batch_toc$tic, 3),
            " secs. \n")

  }

  return(list(lhs = L$lhs, rhs = L$rhs))

}
