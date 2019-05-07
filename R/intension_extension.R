#' Intension of Objects
#'
#' @param cxt      Contexts where to apply intension
#' @param obj_list List or vector with object names
#'
#' @return The intension (derivation) of the given objects
#' @export
#'
#' @examples
#' cxt_file <- system.file("digits.cxt", package = "fcaR")
#' cxt <- context_from_file(cxt_file)
#' cxt %>% get_intension(c("0", "8"))
#'
get_intension <- function(cxt, obj_list) {

  cxt$intension(obj_list) %>% unlist()

}

#' Extension of Properties
#'
#' @param cxt       Contexts where to apply intension
#' @param prop_list List or vector with properties names
#'
#' @return The extension (derivation) of the given properties
#' @export
#'
#' @examples
#' cxt_file <- system.file("digits.cxt", package = "fcaR")
#' cxt <- context_from_file(cxt_file)
#' cxt %>% get_extension(c("c", "d"))
#'
get_extension <- function(cxt, obj_list) {

  cxt$extension(obj_list) %>% unlist()

}
