#' @export
#' @noRd
`[.ConceptSet` <- function(x, ...) x$`[`(...)
#' @export
#' @noRd
`[.ImplicationSet` <- function(x, ...) x$`[`(...)
#' @export
#' @noRd
`[.Set` <- function(x, ...) x$`[`(...)

#' @export
#' @noRd
as.list.ConceptSet <- function(x, ...) {

  if ("to_list" %in% names(x)) {

    return(x$to_list())

  } else {

    stop("Cannot convert R6 class to list...")

  }

}
