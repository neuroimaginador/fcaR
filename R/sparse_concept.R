#' @title
#' R6 class for a fuzzy concept with sparse internal representation
#'
#' @description
#' This class implements the data structure and methods for fuzzy concepts.
#'
#' @examples
#' # Build a formal context and find its concepts
#' fc_planets <- FormalContext$new(planets)
#' fc_planets$find_concepts()
#'
#' # Print the first three concepts
#' fc_planets$concepts[1:3]
#'
#' # Select the first concept:
#' C <- fc_planets$concepts$sub(1)
#'
#' # Get its extent and intent
#' C$get_extent()
#' C$get_intent()
#'
#' @export
Concept <- R6::R6Class(

  classname = "Concept",

  public = list(

    #' @description
    #' Creator for objects of class \code{Concept}
    #'
    #' @param extent  (\code{Set}) The extent of the concept.
    #' @param intent  (\code{Set}) The intent of the concept.
    #'
    #' @return An object of class \code{Concept}.
    #' @export
    initialize = function(extent, intent) {

      stopifnot(inherits(extent, "Set"))
      stopifnot(inherits(intent, "Set"))

      private$extent <- extent
      private$intent <- intent

    },

    #' @description
    #' Internal \code{Set} for the extent
    #'
    #' @return The \code{Set} representation of the extent.
    #'
    #' @export
    get_extent = function() {

      private$extent

    },

    #' @description
    #' Internal \code{Set} for the intent
    #'
    #' @return The \code{Set} representation of the intent.
    #'
    #' @export
    get_intent = function() {

      private$intent

    },

    # is_subconcept = function(S) {
    #
    #   all(private$extent$get_vector() <= S$extent$get_vector())
    #
    # },

    #' @description
    #' Prints the concept to console
    #'
    #' @return A string with the elements of the set and their grades between brackets {}.
    #'
    #' @export
    print = function() {

      cat("(")
      cat(private$extent$print(FALSE))
      cat(", ")
      cat(private$intent$print(FALSE))
      cat(")\n")

    },

    #' @description
    #' Write the concept in LaTeX format
    #'
    #' @param print (logical) Print to output?
    #'
    #' @return The fuzzy concept in LaTeX.
    #' @export
    to_latex = function(print = TRUE) {

      extent <- set_to_latex(private$extent$get_vector(),
                             private$extent$get_attributes())
      intent <- set_to_latex(private$intent$get_vector(),
                             private$intent$get_attributes())

      str <- paste0("\\ensuremath{\\left(", extent, ", ", intent, "\\right)}\n")

      if (print) {

        cat(str)
        return(invisible(str))

      } else {

        return(str)

      }

    }

  ),

  private = list(

    extent = NULL,
    intent = NULL

  )

)
