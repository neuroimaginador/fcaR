# Variable, global to package's namespace.
# This function is not exported to user space and does not need to be documented.
MYPKGOPTIONS <- settings::options_manager(
  logic = "Zadeh",
  Galois_connection = "standard",
  decimal_places = 2,
  latex_size = "normalsize",
  reduced_lattice = TRUE,
  use_mathrm = TRUE,
  use_ensuremath = FALSE,
  use_tabulary = FALSE,
  escape_ = TRUE,
  latex_fraction = "none",
  .allowed = list(
    latex_fraction = settings::inlist("none", "frac", "dfrac", "sfrac", "nicefrac"),
    logic = settings::inlist("Zadeh", "Lukasiewicz"),
    Galois_connection = settings::inlist("standard", "benevolent1", "benevolent2"),
    latex_size = settings::inlist("tiny",
                                  "scriptsize",
                                  "footnotesize",
                                  "small",
                                  "normalsize",
                                  "large",
                                  "Large",
                                  "LARGE",
                                  "huge",
                                  "Huge")
  ))

# User function that gets exported:

#' Set or get options for fcaR
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#' \itemize{
#'  \item{\code{decimal_places}}{(\code{numeric};2) The number of decimal places to show when printing or exporting to \LaTeX sets, implications, concepts, etc.}
#'  \item{\code{latex_size}}{(\code{character};"normalsize") Size to use when exporting to LaTeX.}
#'  \item{\code{reduced\_lattice}}{(\code{logical};TRUE) Plot the reduced concept lattice?}
#' }
#'
#' @export
fcaR_options <- function(...){
  # protect against the use of reserved words.
  settings::stop_if_reserved(...)
  MYPKGOPTIONS(...)
}
