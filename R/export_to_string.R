#' @import stringr
.set_to_string <- function(S, attributes, dictionary) {

  idx <- which(S > 0)

  if (length(idx) > 0) {

    A <- .values_to_terms(S[idx], dictionary = dictionary)
    att <- attributes[idx]

    tmp <- paste0("{",
                  str_flatten(paste0(att, " [", A, "]"),
                              collapse = ", "), "}")

    gsub(pattern = "( \\[1\\])", replacement = "", x = tmp)

  } else {

    "{}"

  }

}

.concept_to_string <- function(C, objects, attributes,
                               dictionary) {

  A <- .set_to_string(C$get_extent()$get_vector(), objects, NULL)
  B <- .set_to_string(C$get_intent()$get_vector(), attributes, dictionary)

  return(paste0("(", A, ", ", B, ")"))

}
