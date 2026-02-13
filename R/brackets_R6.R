#' @export
#' @noRd
`[.ConceptSet` <- function(x, ...) x$`[`(...)
#' @export
#' @noRd
`[.RuleSet` <- function(x, ...) x$`[`(...)
#' @export
#' @noRd
`[.ImplicationSet` <- function(x, ...) x$`[`(...)
#' @export
#' @noRd
`[.Set` <- function(x, ...) x$`[`(...)
#' @export
#' @noRd
`[.FormalContext` <- function(x, ...) x$`[`(...)

#' @export
#' @method as.list ConceptSet
#' @noRd
as.list.ConceptSet <- function(x, ...) {
  if ("to_list" %in% names(x)) {
    return(x$to_list())
  } else {
    stop("Cannot convert R6 class to list...")
  }
}

#' @export
#' @method length ConceptSet
#' @noRd
length.ConceptSet <- function(x, ...) {
  x$size()
}
