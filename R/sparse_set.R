#' @title
#' R6 class for a fuzzy set with sparse internal representation
#'
#' @description
#' This class implements the data structure and methods for fuzzy sets.
#'
#' @examples
#' S <- Set$new(attributes = c("A", "B", "C"))
#' S$assign(A = 1)
#' print(S)
#' S$to_latex()
#'
#' S <- Set$new(c("A", "B", "C"), C = 1, B = 0.5)
#' S
#'
#' @export
Set <- R6::R6Class(

  classname = "Set",

  public = list(

    #' @description
    #' Creator for objects of class \code{Set}
    #'
    #' @param attributes  (character vector) Names of the attributes that will be available in the fuzzy set.
    #' @param ... \code{key} = \code{value} pairs, where the value \code{value} is assigned to the \code{key} attribute name.
    #' @param M           (numeric vector or column \code{Matrix}) Values (grades) to be assigned to the attributes.
    #'
    #' @details
    #' If \code{M} is omitted and no pair \code{key} = \code{value}, the fuzzy set is the empty set. Later, one can use the \code{assign} method to assign grades to any of its attributes.
    #'
    #' @return An object of class \code{Set}.
    #' @export
    initialize = function(attributes, M = NULL, ...) {

      private$attributes <- attributes

      if (!is.null(M)) {

        private$v <- Matrix::Matrix(M, sparse = TRUE)

      } else {

        private$v <- Matrix::Matrix(0,
                                    nrow = length(attributes),
                                    ncol = 1,
                                    sparse = TRUE)

      }

      dots <- list(...)
      if (length(dots) > 0) {

        do.call(self$assign, dots)

      }

    },

    #' @description
    #' Assign grades to attributes in the set
    #'
    #' @param ... \code{key} = \code{value} pairs, where the value \code{value} is assigned to the \code{key} attribute name.
    #' @param attributes  (character vector) Names of the attributes to assign a grade to.
    #' @param values      (numeric vector) Grades to be assigned to the previous \code{attributes}.
    #'
    #' @details
    #' One can use both of:
    #' \code{S$assign(A = 1, B = 0.3)}
    #' \code{S$assign(attributes = c(A, B), values = c(1, 0.3))}.
    #'
    #' @export
    assign = function(attributes = c(),
                      values = c(),
                      ...) {

      dots <- unlist(list(...))

      attrs <- names(dots)
      vals <- unname(dots)

      attributes <- c(attributes, attrs)
      values <- c(values, vals)

      idx <- match(attributes, private$attributes)

      nas <- which(is.na(idx))
      if (length(nas) > 0) {

        idx <- idx[-nas]
        values <- values[-nas]

      }

      if (length(idx) > 0) {

        private$v[idx] <- values

      }

    },

    #' @description
    #' Get elements by index
    #'
    #' @param indices (numeric, logical or character vector) The indices of the elements to return. It can be a vector of logicals where \code{TRUE} elements are to be retained.
    #'
    #' @return A \code{Set} but with only the required elements.
    #'
    #' @export
    `[` = function(indices) {

      if (is.logical(indices)) {

        indices <- which(indices)

      }

      if (is.character(indices)) {

        indices <- match(indices, private$attributes)
        indices <- indices[!is.na(indices)]

      }

      if (is.numeric(indices)) {

        indices <- indices[indices <= self$length()]

      }

      w <- private$v
      idx <- setdiff(seq(self$length()), indices)
      w[idx] <- 0
      S <- Set$new(attributes = private$attributes,
                   M = w)

      return(S)

    },

    #' @description
    #' Cardinal of the Set
    #'
    #' @return the cardinal of the \code{Set}, counted
    #' as the sum of the degrees of each element.
    #'
    #' @export
    cardinal = function() {

      sum(private$v)

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
    #' @param eol (logical) If \code{TRUE}, adds an end of line to the output.
    #'
    #' @return A string with the elements of the set and their grades between brackets {}.
    #'
    #' @export
    print = function(eol = TRUE) {

      if (sum(private$v) > 0) {

        cat(stringr::str_wrap(.set_to_string(S = private$v,
                                             attributes = private$attributes),
                              width = getOption("width"),
                              exdent = 2))
        if (eol) {

          cat("\n")

        } else {

          cat("")

        }

      } else {

        if (eol) {

          cat("{}\n")

        } else {

          cat("{}")

        }

      }

    },

    #' @description
    #' Write the set in LaTeX format
    #'
    #' @param print (logical) Print to output?
    #'
    #' @return The fuzzy set in LaTeX.
    #' @export
    to_latex = function(print = TRUE) {

      str <- "\\ensuremath{\\varnothing}"
      if (sum(private$v) > 0) {

        str <- set_to_latex(S = private$v,
                            attributes = private$attributes)

      }

      if (print) {

        cat(str)
        return(invisible(str))

      } else {

        return(str)

      }

    }

  ),

  private = list(

    v = NULL,

    attributes = NULL

  )

)
