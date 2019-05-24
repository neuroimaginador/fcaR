#' @import sets
implication <- R6::R6Class(

  classname = "Implication",

  public = list(

    initialize = function(lhs = gset(), rhs = gset()) {

      stopifnot(gset_is_set_or_fuzzy_set(lhs))
      stopifnot(gset_is_set_or_fuzzy_set(rhs))

      private$lhs <- lhs
      private$rhs <- rhs

    },

    print = function() {

      cat(.gset_to_string(private$lhs),
          " -> ",
          .gset_to_string(private$rhs),
          "\n")

    },

    is_respected = function(A) {

      !(gset_is_subset(private$lhs, A)) ||
      gset_is_subset(gset_union(private$lhs,
                                private$rhs),
                     A)

    },

    compute_closure = function(A) {

      changed <- FALSE
      A_orig <- A

      if (private$lhs <= A) {

        A <- gset_union(A, private$rhs)

        changed <- !(gset_is_equal(A, A_orig))

      }

      return(list(changed = changed, closure = A))

    },

    get_lhs = function() {

      private$lhs

    },

    get_rhs = function() {

      private$rhs

    }

  ),

  private = list(

    lhs = gset(),
    rhs = gset()

  )

)
