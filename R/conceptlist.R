#' @export
#' @noRd
print.conceptlist <- function(x) {

  for (i in seq_along(x)) {

    x[[i]]$print()

  }

}

#' @export
#' @noRd
to_latex <- function(x) UseMethod("to_latex")

#' @export
#' @noRd
to_latex.conceptlist <- function(x) {

  str <- concepts_to_latex(x, numbered = FALSE)

  cat(str)

}
