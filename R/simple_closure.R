.implications_closure <- function(S, implication_set) {

  new_closure <- S
  unused_implications <- implication_set

  changed <- TRUE

  while (changed) {

    changed <- FALSE

    for (imp_idx in seq_along(unused_implications)) {

      imp <- implication_set[[imp_idx]]

      if (length(imp) > 0) {

        c(changed, new_closure) := imp$compute_closure(new_closure)

        if (changed) unused_implications[imp_idx] <- c()

      }

    }

  }

  return(new_closure)

}
