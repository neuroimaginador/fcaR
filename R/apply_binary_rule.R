.apply_binary_rule_once <- function(implication_list,
                                    rule = .composition) {

  any_changed <- FALSE

  marked_to_remove <- rep(FALSE, length(implication_list))

  for (i in rev(seq_along(implication_list))) {

    if (marked_to_remove[i]) next

    for (j in rev(seq_along(implication_list))) {

      if (j == i) next

      if (marked_to_remove[j]) next

      imp1 <- implication_list[[i]]
      imp2 <- implication_list[[j]]

      L <- rule(imp1, imp2)
      changed <- L$changed
      imp1 <- L$imp1
      imp2 <- L$imp2

      if (changed) {

        any_changed <- TRUE
        implication_list[[i]] <- imp1

        if (!is.null(imp2)) {

          implication_list[[j]] <- imp2

        } else {

          marked_to_remove[j] <- TRUE

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

.apply_binary_rule <- function(implication_list,
                              rule = .reduction) {

  changed <- TRUE

  while (changed) {

    L <-  .apply_binary_rule_once(implication_list,
                                  rule)

    changed <- L$changed
    implication_list <- L$implication_list


  }

  return(implication_list)

}
