#' @title
#' R6 class for a fuzzy set with sparse internal representation
#'
#' @description
#' This class implements the data structure and methods for fuzzy sets.
#'
#' @importFrom methods as is slotNames
#' @import Matrix
#'
#' @export
sparse_set <- R6::R6Class(

  classname = "SparseSet",

  public = list(

    #' @description
    #' Creator for objects of class \code{SparseSet}
    #'
    #' @param attributes  (character vector) Names of the attributes that will be available in the fuzzy set.
    #' @param M           (numeric vector or column \code{Matrix}) Values (grades) to be assigned to the attributes.
    #'
    #' @details
    #' If \code{M} is omitted, the fuzzy set is the empty set. Later, one can use the \code{assign} method to assign grades to any of its attributes.
    #'
    #' @return An object of class \code{SparseSet}.
    #' @export
    initialize = function(attributes, M = NULL) {

      private$attributes <- attributes

      if (!is.null(M)) {

        private$v <- Matrix(M, sparse = TRUE)

      } else {

        private$v <- Matrix(0,
                            nrow = length(attributes),
                            ncol = 1,
                            sparse = TRUE)

      }

    },

    #' @description
    #' Assign grades to attributes in the set
    #'
    #' @param attributes  (character vector) Names of the attributes to assign a grade to.
    #' @param values      (numeric vector) Grades to be assigned to the previous \code{attributes}.
    #'
    #' @export
    assign = function(attributes, values) {

      idx <- match(attributes, private$attributes)

      private$v[idx] <- values

    },

    #' @description
    #' Internal \code{Matrix}
    #'
    #' @return The internal sparse \code{Matrix} representation of the set.
    #'
    #' @export
    get_vector = function() {

      private$v

    },

    #' @description
    #' Attributes defined for the set
    #'
    #' @return A character vector with the names of the attributes.
    #'
    #' @export
    get_attributes = function() {

      private$attributes

    },

    #' @description
    #' Number of attributes
    #'
    #' @return The number of attributes that are defined for this fuzzy set.
    #'
    #' @export
    length = function() {

      length(private$attributes)

    },

    #' @description
    #' Prints the set to console
    #'
    #' @return A string with the elements of the set and their grades between brackets {}.
    #'
    #' @importFrom stringr str_wrap
    #' @export
    print = function() {

      if (sum(private$v) > 0) {

        cat(str_wrap(.set_to_string(S = private$v,
                           attributes = private$attributes),
                     width = 75,
                     exdent = 2))

      } else {

        cat("{}")

      }

    },

    #' @description
    #' Write the set in LaTeX format
    #'
    #' @return The fuzzy set in LaTeX.
    #' @export
    to_latex = function() {

      str <- "\\ensuremath{\\emptyset}"
      if (sum(private$v) > 0) {

        str <- set_to_latex(S = private$v,
                            attributes = private$attributes)

      }

      return(str)

    }

  ),

  private = list(

    v = NULL,

    attributes = NULL

  )

)
