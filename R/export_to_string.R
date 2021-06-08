.set_to_string <- function(S, attributes) {

  idx <- Matrix::which(S > 0)

  if (length(idx) > 0) {

    A <- S[idx]
    att <- attributes[idx]

    tmp <- paste0("{",
                  stringr::str_flatten(paste0(att, " [", A, "]"),
                              collapse = ", "), "}")

    gsub(pattern = "( \\[1\\])", replacement = "", x = tmp)

  } else {

    "{}"

  }

}

.old.concept_to_string <- function(C, objects, attributes) {

  A <- .set_to_string(C$get_extent()$get_vector(), objects)
  B <- .set_to_string(C$get_intent()$get_vector(), attributes)

  return(paste0("(", A, ", ", B, ")"))

}

.concept_to_string <- function(vA, vB, objects, attributes) {

  A <- .set_to_string(vA, objects)
  B <- .set_to_string(vB, attributes)

  return(paste0("(", A, ", ", B, ")"))

}
