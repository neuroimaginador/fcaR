#' @import arules
.is_redundant_binary <- function(LHS, RHS, I) {

  LHS <- as.matrix(LHS)
  RHS <- as.matrix(RHS)
  I <- as.matrix(I)
  grades_set <- sort(unique(as.vector(I)))

  I <- .expand_dataset(I,
                       grades_set = grades_set,
                       implications = FALSE)


  LHS <- t(.expand_dataset(t(LHS),
                           grades_set = grades_set,
                           implications = FALSE))
  RHS <- t(.expand_dataset(t(RHS),
                           grades_set = grades_set,
                           implications = FALSE))

  RHS[LHS >= RHS] <- 0

  LHS <- as(LHS, "ngCMatrix")
  LHS <- as(LHS, "itemMatrix")
  itemLabels(LHS) <- paste0(seq(ncol(LHS)))

  RHS <- as(RHS, "ngCMatrix")
  RHS <- as(RHS, "itemMatrix")
  itemLabels(RHS) <- paste0(seq(ncol(RHS)))

  rules <- new("rules", lhs = LHS, rhs = RHS)

  my_transactions <- as(as(t(I), "ngCMatrix"), "transactions")

  # Note: improvement is defined for confidence,
  # but could also used with other measures
  q <- interestMeasure(rules, "improvement", my_transactions, TRUE)
  imp <- numeric(length(rules))

  # do it by unique rhs
  rr <- .Call("R_pnindex",
              RHS@data,
              NULL,
              FALSE,
              PACKAGE = "arules")

  for (r in unique(rr)) {

    pos <- which(rr == r)

    q2 <- q[pos]

    if (length(pos) > 1) {

      ### FALSE is for verbose
      qsubmax <- .Call("R_pnmax",
                       LHS@data[, pos],
                       q2,
                       FALSE,
                       PACKAGE = "arules")


    } else {

      qsubmax <- 0

    }

    imp[pos] <- q2 - qsubmax

  }

  redundant <- imp <= 0

}
