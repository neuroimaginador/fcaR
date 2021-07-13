#' Tamaño
#'
#' @param implications  This is the set of implications
#'
#' @return
#' The size of the set of implications
#' @export
#'
#' @examples
#' library(fcaR)
#' fc <- FormalContext$new(planets)
#' fc$find_implications()
#' mitotal <- tamaño(fc$implications)
tamaño <- function(implications){
  # calcula el tamaño de un conjunto de implicaciones
  total <- sum(implications$size())
  return(total)
}

