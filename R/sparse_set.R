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

    assign = function(attributes, values) {

      idx <- match(attributes, private$attributes)

      private$v[idx] <- values

    },

    get_vector = function() {

      private$v

    },

    get_attributes = function() {

      private$attributes

    },

    length = function() {

      length(private$attributes)

    },

    print = function() {

      if (sum(private$v) > 0) {

        cat(print_set(A = private$v,
                      attributes = private$attributes))

      } else {

        cat("{}\n")

      }

    },

    to_latex = function() {

      if (sum(private$v) > 0) {

        cat(set_to_latex(S = private$v,
                         attributes = private$attributes))

      } else {

        cat("\\emptyset")

      }

    }

  ),

  private = list(

    v = NULL,

    attributes = NULL

  )

)
