#' @export
formal_context <- R6::R6Class(

  classname = "FormalContext",

  public = list(

    I = NULL,

    attributes = NULL,

    objects = NULL,

    grades_set = NULL,

    concepts = NULL,

    initialize = function(I, grades_set) {

      stopifnot(length(colnames(I)) == ncol(I))
      stopifnot(length(rownames(I)) == nrow(I))

      self$I <- I
      self$grades_set <- grades_set

      self$objects <- rownames(I)
      self$attributes <- colnames(I)

    },

    get_concepts = function() {

      if (!is.null(self$concepts)) return(self$concepts)

      self$concepts <- .get_fuzzy_concepts(self$I,
                                           self$grades_set)

      return(self$concepts)

    },

    plot = function() {

      if (length(self$concepts) > 0) {

        .draw_Hasse(self$concepts, self$I)

      }
    }

  )

)
