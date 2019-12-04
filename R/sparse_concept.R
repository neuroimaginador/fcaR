#' @title
#' R6 class for a fuzzy concept with sparse internal representation
#'
#' @description
#' This class implements the data structure and methods for fuzzy concepts.
#'
#' @importFrom methods as is slotNames
#' @import Matrix
#'
#' @export
SparseConcept <- R6::R6Class(

  classname = "SparseConcept",

  public = list(

    #' @description
    #' Creator for objects of class \code{SparseConcept}
    #'
    #' @param extent  (\code{SparseSet}) The extent of the concept.
    #' @param intent  (\code{SparseSet}) The intent of the concept.
    #'
    #' @return An object of class \code{SparseConcept}.
    #' @export
    initialize = function(extent, intent) {

      stopifnot(inherits(extent, "SparseSet"))
      stopifnot(inherits(intent, "SparseSet"))

      private$extent <- extent
      private$intent <- intent

    },

    #' @description
    #' Internal \code{SparseSet} for the extent
    #'
    #' @return The \code{SparseSet} representation of the extent.
    #'
    #' @export
    get_extent = function() {

      private$extent

    },

    #' @description
    #' Internal \code{SparseSet} for the intent
    #'
    #' @return The \code{SparseSet} representation of the intent.
    #'
    #' @export
    get_intent = function() {

      private$intent

    },

    #' @description
    #' Prints the concept to console
    #'
    #' @return A string with the elements of the set and their grades between brackets {}.
    #'
    #' @export
    print = function() {

      cat("(")
      cat(private$extent$print())
      cat(", ")
      cat(private$intent$print())
      cat(")\n")

    },

    #' @description
    #' Write the concept in LaTeX format
    #'
    #' @return The fuzzy concept in LaTeX.
    #' @export
    to_latex = function() {

      extent <- private$extent$to_latex()
      intent <- private$intent$to_latex()

      str <- paste0("(", extent, ", ", intent, ")\n")

      return(str)

    }

  ),

  private = list(

    extent = NULL,
    intent = NULL

  )

)
