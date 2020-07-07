#' @importFrom stringr str_flatten
#' @importFrom Matrix which
.set_to_string <- function(S, attributes) {

  idx <- which(S > 0)

  if (length(idx) > 0) {

    A <- S[idx]
    att <- attributes[idx]

    tmp <- paste0("{",
                  str_flatten(paste0(att, " [", A, "]"),
                              collapse = ", "), "}")

    gsub(pattern = "( \\[1\\])", replacement = "", x = tmp)

  } else {

    "{}"

  }

}

.concept_to_string <- function(C, objects, attributes) {

  A <- .set_to_string(C$get_extent()$get_vector(), objects)
  B <- .set_to_string(C$get_intent()$get_vector(), attributes)

  return(paste0("(", A, ", ", B, ")"))

}
