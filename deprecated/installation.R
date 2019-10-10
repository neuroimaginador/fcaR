#' Install Python `'Concepts' Framework
#'
#' @return The 'concepts' module
#' @export
#' @import reticulate
#'
install_concepts <- function() {

  if (!py_module_available("concepts")) {

    py_install(packages = "concepts")

  }

  return(invisible(TRUE))

}
