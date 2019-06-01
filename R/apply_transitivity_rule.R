.apply_transitivity_rule_once <- function(implication_list,
                                          rule = .generalized_transitivity) {

  any_changed <- FALSE

  marked_to_remove <- rep(FALSE, length(implication_list))

  for (i in rev(seq_along(implication_list))) {

    if (marked_to_remove[i]) next

    for (j in rev(seq_along(implication_list))) {

      if (j == i) next

      if (marked_to_remove[j]) next

      for (k in rev(seq_along(implication_list))) {

        if ((k == i) || (k == j)) next

        if (marked_to_remove[k]) next

        imp1 <- implication_list[[i]]
        imp2 <- implication_list[[j]]
        imp3 <- implication_list[[k]]

        changed <- rule(imp3, imp1, imp2)

        if (changed) {

          any_changed <- TRUE
          marked_to_remove[k] <- TRUE

        }

      }

    }

  }

  for (i in rev(seq_along(implication_list))) {

    if (marked_to_remove[i]) {

      implication_list[i] <- NULL

    }

  }

  return(list(changed = any_changed,
              implication_list = implication_list))

}

.apply_transitivity_rule <- function(implication_list,
                                     rule = .generalized_transitivity) {

  changed <- TRUE

  while (changed) {

    L <-  .apply_transitivity_rule_once(implication_list,
                                        rule)

    changed <- L$changed
    implication_list <- L$implication_list


  }

  return(implication_list)

}
