#' Print a list of concepts
#'
#' @param x A list of concepts as a result of subsetting a \code{ConceptLattice}
#' @param ... Another (unused) arguments.
#'
#' @return Nothing, just prints a list of concepts nicely.
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
#' @export
to_latex.conceptlist <- function(x) {

  str <- concepts_to_latex(x, numbered = FALSE)

  cat(str)

  return(invisible(NULL))

}

.S3methods("to_latex", "conceptlist")
.S3methods("print", "conceptlist")
