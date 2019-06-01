.apply_unary_rule_once <- function(implication_list,
                              rule = .reduction) {

  any_changed <- FALSE

  for (i in rev(seq_along(implication_list))) {

    imp <- implication_list[[i]]

    L <- rule(imp)
    changed <- L$changed
    new_imp <- L$implication

    if (changed) {

      any_changed <- TRUE
      implication_list[[i]] <- new_imp

    }

  }

  return(list(changed = any_changed,
              implication_list = implication_list))

}

.apply_unary_rule <- function(implication_list,
                              rule = .reduction) {

  changed <- TRUE

  while (changed) {

    L <- .apply_unary_rule_once(implication_list,
                                rule)
    changed <- L$changed
    implication_list <- L$implication_list

  }

  return(implication_list)

}
