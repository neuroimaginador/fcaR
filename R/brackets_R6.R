#' @export
#' @noRd
`[.R6` <- function(x, ...) x$`[`(...)

#' @export
#' @noRd
`[<-.R6` <- function(x, value) x$`[<-`(value)
