#' Launch the fcaRviz Interactive Viewer
#'
#' This function opens the interactive Shiny application fcaRviz for visual exploration.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}.
#' @export
#' @importFrom shiny runApp
#' @examples
#' \dontrun{
#' run_fcaRviz()
#' }
run_fcaRviz <- function(...) {
  # Localizar la ruta de la aplicación instalada en el sistema
  app_dir <- system.file("shiny-examples", "fcaRviz", package = "fcaR")

  if (app_dir == "") {
    stop("Could not find the shiny-examples/fcaRviz directory in the fcaR package. Try re-installing `fcaR`.", call. = FALSE)
  }

  # Ejecutar la aplicación Shiny
  shiny::runApp(app_dir, ...)
}
