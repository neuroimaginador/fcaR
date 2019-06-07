#' @import tictoc
.batch_apply <- function(LHS, RHS,
                         rules = c("generalization",
                                   "composition"),
                         batch_size = ncol(LHS),
                         reorder = TRUE) {

  n_implications <- ncol(LHS)

  if (reorder) {

    # Introduce some random order into implications
    ordering <- sample(seq(n_implications),
                       size = n_implications)

    LHS <- LHS[, ordering]
    RHS <- RHS[, ordering]

  }

  # Initialize results
  new_LHS <- Matrix(0,
                    nrow = nrow(LHS),
                    ncol = 1,
                    sparse = TRUE)

  new_RHS <- Matrix(0,
                    nrow = nrow(LHS),
                    ncol = 1,
                    sparse = TRUE)

  # Make batches
  idx <- c(seq(1, n_implications, by = batch_size),
           n_implications + 1)


  my_functions <- c("generalization" = .remove_redundancies_lhs_rhs_general,
                    "composition"    = .compose_lhs_rhs_equal,
                    "reduction"      = .reduce_lhs_rhs,
                    "simplification" = .simplify_lhs_rhs,
                    "simpl_ijar"     = .simplify_IJAR_lhs_rhs)

  idx_rules <- match(rules, names(my_functions))
  idx_rules <- idx_rules[!is.na(idx_rules)]
  rules_to_apply <- my_functions[idx_rules]
  rule_names <- names(my_functions)[idx_rules]

  # In each batch, use the needed functions
  for (i in seq_along(idx[-1])) {

    # Begin the timing
    tic("batch")

    cat("Processing chunk", i, "out of", length(idx) - 1, "\n")

    old_LHS <- LHS[, idx[i]:(idx[i + 1] - 1)]
    old_RHS <- RHS[, idx[i]:(idx[i + 1] - 1)]
    new_cols <- idx[i + 1] - idx[i]

    # Loop over all functions
    for (j in seq_along(idx_rules)) {

      current_cols <- new_cols

      current_rule <- rules_to_apply[[j]]

      tic("rule")
      L <- current_rule(old_LHS, old_RHS)

      rule_time <- toc(quiet = TRUE)
      old_LHS <- L$lhs
      old_RHS <- L$rhs

      new_cols <- ncol(old_LHS)

      cat("-->", rule_names[j], ": from", current_cols, "to",
          new_cols, "in", rule_time$toc - rule_time$tic, "secs. \n")

    }

    # Add the computed implications to the set
    new_LHS <- cbind(new_LHS, old_LHS)
    new_RHS <- cbind(new_RHS, old_RHS)

    batch_toc <- toc(quiet = TRUE)

    cat("Batch took", batch_toc$toc - batch_toc$tic, "secs. \n")

  }

  LHS <- new_LHS[, -1]
  RHS <- new_RHS[, -1]

  return(list(lhs = LHS, rhs = RHS))

}
