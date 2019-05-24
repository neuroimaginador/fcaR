implication_set <- R6::R6Class(

  classname = "ImplicationSet",

  public = list(

    # Initialize with an optional name
    initialize = function(name = "") {

      private$name <- name

    },

    # Number of implications in the set
    length = function() {

      length(private$implications)

    },

    # Add new implication
    add_implication = function(imp) {

      private$implications <- c(private$implications, imp)

    },

    get_implications = function() {

      private$implications

    },

    # Compute the sintactic closure of a set wrt the implications
    compute_closure = function(S) {

      .implications_closure(S, private$implications)

    },

    print = function() {

      n <- 1
      for (imp in private$implications) {

        cat("Rule ", n, ": ")
        imp$print()
        n <- n + 1

      }

    },

    reduce = function() {

      changed <- TRUE
      while (changed) {

        changed <- FALSE

        for (imp_idx in rev(seq_along(private$implications))) {

          imp <- private$implications[[imp_idx]]

          old_rhs <- imp$get_rhs()

          imp <- .reduceAinB(lhs = imp$get_lhs(),
                             rhs = imp$get_rhs())

          if (gset_is_empty(imp$get_rhs())) {

            changed <- TRUE

            private$implications[imp_idx] <- NULL

          } else {

            if (!(old_rhs == imp$get_rhs())) {

              changed <- TRUE

            }

            private$implications[[imp_idx]] <- imp

          }

        }

      }

    }

  ),

  private = list(

    name = "",

    implications = list()

  )

)
