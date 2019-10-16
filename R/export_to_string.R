#' @import stringr
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

  A <- .set_to_string(C[[1]], objects)
  B <- .set_to_string(C[[2]], attributes)

  return(paste0("[", A, ", ", B, "]"))

}
