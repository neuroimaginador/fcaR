.set_to_string <- function(S, attributes) {

  idx <- Matrix::which(S > 0)

  if (length(idx) > 0) {

    A <- S[idx]
    decimal_places <- fcaR_options("decimal_places")
    A <- A %>%
      formatC(digits = decimal_places) %>%
      stringr::str_replace_all("\\s*", "")
    att <- attributes[idx]

    tmp <- paste0("{",
                  stringr::str_flatten(paste0(att, " [", A, "]"),
                              collapse = ", "), "}")

    gsub(pattern = "( \\[1\\])", replacement = "", x = tmp)

  } else {

    "{}"

  }

}

.concept_to_string <- function(vA, vB, objects, attributes) {

  A <- .set_to_string(vA, objects)
  B <- .set_to_string(vB, attributes)

  return(paste0("(", A, ", ", B, ")"))

}
