#' Export to CXT format
#'
#' @param I           (Binary) Incidence matrix
#' @param objects     Objects of the context
#' @param attributes  Attributes of the context
#' @param filename    Path to the file where to save the CXT format.
#'
#' @return \code{TRUE} invisibly.
#'
#' @noRd
#'
to_cxt <- function(I, objects, attributes,
                   filename) {

  output <- c("B", "",
              length(objects), length(attributes), "",
              objects, attributes)

  str <- I |>
    .print_binary() |>
    apply(1, stringr::str_flatten) |>
    stringr::str_replace_all(pattern = stringr::fixed(" "),
                             replacement = ".")

  output <- c(output, str)

  cat(output, sep = "\n", file = filename)

  return(invisible(TRUE))

}
