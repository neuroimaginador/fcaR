.apply_rule <- function(implication_list, rule) {

  N <- length(formals(rule))

  if (N == 1) {

    implication_list <- .apply_unary_rule(implication_list,
                                          rule)

  } else {

    if (N == 2) {

      implication_list <- .apply_binary_rule(implication_list,
                                             rule)

    } else {

      implication_list <- .apply_transitivity_rule(implication_list,
                                                   rule)

    }

  }

  return(implication_list)

}
