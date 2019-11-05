.get_sublattice <- function(L, starting_idx) {

  idx <- starting_idx
  idx_final <- starting_idx

  M <- .concept_order(L)

  # joins
  added <- TRUE
  while (added) {

    if (length(idx_final) == length(L)) break;

    added <- FALSE
    to_add <- c()
    for (i in idx) {

      for (j in idx) {

        if (i != j) {

          v <- M[i, ] & M[j, ]

          id_join <- which(v)

          if (length(id_join) == 1) {

            id_add <- setdiff(id_join, c(idx_final, to_add))

          } else {

            id_add <- which(rowSums(M[id_join, ]) == length(id_join))
            id_add <- setdiff(id_join[id_add], c(idx_final, to_add))

          }

          if (length(id_add) > 0) {

            added <- TRUE
            to_add <- c(to_add, id_add)

          }

        }

      }

    }

    idx_final <- sort(c(idx_final, to_add))
    idx <- to_add

  }

  # meet
  added <- TRUE
  idx <- idx_final
  while (added) {

    if (length(idx_final) == length(L)) break;

    added <- FALSE
    to_add <- c()
    for (i in idx) {

      for (j in idx) {

        if (i != j) {

          v <- M[, i] & M[, j]

          id_join <- which(v)

          if (length(id_join) == 1) {

            id_add <- setdiff(id_join, c(idx_final, to_add))

          } else {

            id_add <- which(colSums(M[, id_join]) == 1)
            id_add <- setdiff(id_join[id_add], c(idx_final, to_add))

          }

          if (length(id_add) > 0) {

            added <- TRUE
            to_add <- c(to_add, id_add)

          }

        }

      }

    }

    idx_final <- sort(c(idx_final, to_add))
    idx <- to_add

  }

  return(L[idx_final])

}
