#' Create a Context from File
#'
#' @param filename (character) The CSV o CXT file
#'
#' @return A context object
#' @export
#' @import reticulate
#'
#' @examples
#'
#' filename <- system.file("digits.cxt", package = "fcaR")
#' cxt <- context_from_file(filename)
#'
context_from_file <- function(filename) {

  # Initialize context
  concepts <- import("concepts")

  cxt <- NULL

  # Tries to guess the format
  switch(toupper(tools::file_ext(filename)),

         "CXT" = {

           cxt <- concepts$load_cxt(filename)

         },

         "CSV" = {

           cxt <- concepts$load_csv(filename)

         })

  return(cxt)

}
