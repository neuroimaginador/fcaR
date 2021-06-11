#' Print a list of concepts
#'
#' @param x A list of concepts as a result of subsetting a \code{ConceptLattice}
#' @param ... Another (unused) arguments.
#'
#' @return Nothing, just prints a list of concepts nicely.
#'
#' @examples
#' # Build a formal context
#' fc_planets <- FormalContext$new(planets)
#'
#' # Find its concepts
#' fc_planets$find_concepts()
#'
#' # Print the first 3 concepts
#' fc_planets$concepts[1:3]
#'
#' @rdname print
#' @method print conceptlist
#' @export
print.conceptlist <- function(x, ...) {

  for (i in seq_along(x)) {

    x[[i]]$print()

  }

  return(invisible(NULL))

}


#' Print a list of concepts to LaTeX
#'
#' @param x A \code{conceptlist} as result of subsetting a \code{ConceptLattice}
#'
#' @return
#' Nothing, just prints a list of concepts in LaTeX nicely.
#'
#' @export
to_latex <- function(x) UseMethod("to_latex")

#'
#' @rdname to_latex
#' @method to_latex conceptlist
#'
#' @examples
#' # Build a formal context
#' fc_planets <- FormalContext$new(planets)
#'
#' # Find its concepts
#' fc_planets$find_concepts()
#'
#' # Print the first 3 concepts to latex
#' to_latex(fc_planets$concepts[1:3]$to_list())
#'
#' @export
to_latex.conceptlist <- function(x) {

  for (concept in x) {

    concept$to_latex()

  }

  return(invisible(NULL))

}

# .S3methods("to_latex", "conceptlist")
# .S3methods("print", "conceptlist")
