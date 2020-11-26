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

  LHS <- methods::as(L$lhs, "ngCMatrix")
  LHS <- methods::as(LHS, "itemMatrix")
  arules::itemLabels(LHS) <- attributes

  RHS <- methods::as(L$rhs, "ngCMatrix")
  RHS <- methods::as(RHS, "itemMatrix")
  arules::itemLabels(RHS) <- attributes

  rules <- methods::new("rules", lhs = LHS, rhs = RHS)

  # This is needed in arules from version 1.6-6
  # Solves issue #15 by Michael Hahsler
  arules::info(rules) <- list(data = "",
                              support = 0,
                              confidence = 1,
                              ntransactions = ncol(I))

  if (quality) {

    arules::quality(rules) <- arules::interestMeasure(rules,
                                                      transactions = methods::as(methods::as(I, "ngCMatrix"), "transactions"))

  }

  return(rules)

}
