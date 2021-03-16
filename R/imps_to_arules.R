imps_to_arules <- function(LHS, RHS, attributes,
                           I, quality = TRUE) {

  if (!requireNamespace("arules", quietly = TRUE)) {

    stop("Package 'arules' is not available.",
         call. = FALSE)

  }

  # Needed to export to arules
  L <- .reduction(LHS = LHS,
                  RHS = RHS,
                  attributes = attributes)

  LHS <- Matrix::sparseMatrix(i = L$lhs$pi,
                              p = L$lhs$pp,
                              x = L$lhs$px,
                              dims = c(L$lhs$pnrow, length(L$lhs$pp) - 1)) %>%
    methods::as("ngCMatrix")
  LHS <- methods::as(LHS, "itemMatrix")
  arules::itemLabels(LHS) <- attributes

  RHS <- Matrix::sparseMatrix(i = L$rhs$pi,
                              p = L$rhs$pp,
                              x = L$rhs$px,
                              dims = c(L$rhs$pnrow, length(L$rhs$pp) - 1)) %>%
    methods::as("ngCMatrix")
  RHS <- methods::as(RHS, "itemMatrix")
  arules::itemLabels(RHS) <- attributes

  rules <- methods::new("rules", lhs = LHS, rhs = RHS)

  # This is needed in arules from version 1.6-6
  # Solves issue #15 by Michael Hahsler
  arules::info(rules) <- list(data = "",
                              support = 0,
                              confidence = 1,
                              ntransactions = ncol.SpM(I))

  if (quality) {

    arules::quality(rules) <- arules::interestMeasure(rules,
                                                      transactions = to_transactions.SpM(I))

  }

  return(rules)

}
