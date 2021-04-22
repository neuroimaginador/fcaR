.get_sublattice <- function(M, starting_idx) {

  idx <- starting_idx
  idx_final <- starting_idx

  N <- ncol(M)

  # joins
  added <- TRUE
  while (added) {

    if (length(idx_final) == N) break;

    added <- FALSE
    to_add <- c()
    for (i in idx) {

      for (j in idx) {

        if (i != j) {

          id_join <- join(M, c(i, j))

          id_add <- setdiff(id_join, c(idx_final, to_add))

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

  idx1 <- idx
  # meet
  added <- TRUE
  idx <- starting_idx
  while (added) {

    if (length(idx_final) == N) break;

    added <- FALSE
    to_add <- c()
    for (i in idx) {

      for (j in idx) {

        if (i != j) {

          id_join <- meet(M, c(i, j))

          id_add <- setdiff(id_join, c(idx_final, to_add))

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

  idx_final <- sort(c(idx1, idx_final))
  return(idx_final)

}
