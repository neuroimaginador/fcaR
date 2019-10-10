initialize_implication_tree <- function(n_attributes) {

  LIST <- list()
  COUNT <- c()
  DEGREE <- list()
  SKIP <- list()
  CARD <- c()

  for (att in seq(n_attributes)) {

    LIST[[att]] <- vector(mode = "integer")
    SKIP[[att]] <- vector()
    DEGREE[[att]] <- vector(mode = "numeric")

  }

  tree <- list(LIST = LIST, SKIP = SKIP,
               DEGREE = DEGREE, COUNT = COUNT,
               CARD = CARD,
               n_implications = 0,
               n_attributes = n_attributes)

  return(as.environment(tree))

}

add_implications_tree <- function(tree, LHS) {

  n_implications_to_add <- ncol(LHS)

  if (n_implications_to_add > 0) {

    tLHS <- t(LHS)
    partialCOUNT <- rep(0, n_implications_to_add)
    partialCARD <- rep(0, n_implications_to_add)

    for (att in seq(tree$n_attributes)) {

      idx_att <- which_at_col(tLHS, att)

      if (length(idx_att) > 0) {

        v <- tLHS[idx_att, att]

        tree$LIST[[att]] <- c(tree$LIST[[att]],
                              tree$n_implications + idx_att)
        tree$SKIP[[att]] <- c(tree$SKIP[[att]],
                              rep(FALSE, length(idx_att)))
        tree$DEGREE[[att]] <- c(tree$DEGREE[[att]], v)
        partialCOUNT[idx_att] <- partialCOUNT[idx_att] + 1
        partialCARD[idx_att] <- partialCARD[idx_att] + v

      }

    }

    tree$COUNT <- c(tree$COUNT, partialCOUNT)
    tree$CARD <- c(tree$CARD, partialCARD)
    tree$n_implications <- tree$n_implications + n_implications_to_add

  }

  return(tree)

}

.is_subset_tree <- function(S, tree,
                            blacklist = vector()) {

  attrs <- S@i + 1
  values <- S@x

  LIST <- tree$LIST
  DEGREE <- tree$DEGREE
  COUNT <- tree$COUNT

  if (length(values) > 0) {

    for (idx_att in seq_along(attrs)) {

      y <- attrs[idx_att]
      a <- values[idx_att]

      idx <- DEGREE[[y]] <= a & !blacklist[LIST[[y]]]
      COUNT[LIST[[y]][idx]] <- COUNT[LIST[[y]][idx]] - 1

    }

  }

  return(which(COUNT == 0))

}
