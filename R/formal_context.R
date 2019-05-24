#' @export
#' @import scales RColorBrewer
formal_context <- R6::R6Class(

  classname = "FormalContext",

  public = list(

    I = NULL,

    attributes = NULL,

    objects = NULL,

    grades_set = NULL,

    concepts = NULL,

    implications = NULL,

    initialize = function(I, grades_set) {

      stopifnot(length(colnames(I)) == ncol(I))
      stopifnot(length(rownames(I)) == nrow(I))

      self$I <- I
      self$grades_set <- grades_set

      self$objects <- rownames(I)
      self$attributes <- colnames(I)

    },

    compute_concepts = function(verbose = FALSE) {

      if (!is.null(self$concepts)) return(self$concepts)

      self$concepts <- .get_fuzzy_concepts(self$I,
                                           self$grades_set,
                                           verbose = verbose)

      return(self$concepts)

    },

    extract_implications_concepts = function(verbose = FALSE) {

      c(concepts, implications) := .get_concepts_implications(self$I,
                                                              self$grades_set,
                                                              verbose = verbose)

      self$concepts <- concepts
      self$implications <- implications

    },

    plot_lattice = function() {

      if (length(self$concepts) > 0) {

        .draw_Hasse(self$concepts, self$I)

      }

    },

    plot_context = function() {

      color_function <- colour_ramp(brewer.pal(11, "Greys"))
      heatmap(self$I, Rowv = NA, Colv = NA,
              col = color_function(seq(0, 1, 0.01)),
              scale = "none")

    },

    iceberg_lattice = function(minsupp) {

      if (length(self$concepts) > 0) {

        concept_support <- sapply(self$concepts,
                                  function(s) {

                                    .intent_support(s[[2]], self$I)

                                  })

        idx <- which(concept_support >= minsupp)

        .draw_Hasse(self$concepts[idx], self$I)

        return(self$concepts[idx])

      }

    }

  )

)
