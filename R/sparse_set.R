sparse_set <- R6::R6Class(

  classname = "SparseSet",

  public = list(

    initialize = function(attributes, M = NULL) {

      private$attributes <- attributes

      if (!is.null(M)) {

        private$v <- Matrix(M, sparse = TRUE)

      }

    },

    zero = function() {

      private$v <- 0 * private$v

    },

    assign = function(attributes, values) {

      private$v <- private$v +
        build_set(attrs = attributes,
                  values = values,
                  attributes = private$attributes)

    },

    get_vector = function() {

      private$v

    },

    get_attributes = function() {

      private$attributes

    },

    ncol = function() {

      ncol(private$v)

    },

    nrow = function() {

      nrow(private$v)

    },

    dim = function() {

      c(self$nrow(), self$ncol())

    },

    print = function() {

      if (!is.null(self$ncol())) {

        cat(print_set(A = private$v,
                      attributes = private$attributes))

      } else {

        cat("{}\n")

      }

    },

    to_latex = function() {

      if (!is.null(self$ncol())) {

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
