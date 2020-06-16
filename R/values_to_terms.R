.values_to_terms <- function(values, dictionary) {

  n_terms <- length(dictionary)

  if (n_terms > 0) {

    terms <- rep("", length(values))
    my_terms <- names(dictionary)

    for (i in seq(n_terms)) {

      terms[values >= dictionary[i]] <- my_terms[i]

    }

    return(terms)

  } else {

    return(values)

  }

}
