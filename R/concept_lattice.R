#' @export
ConceptLattice <- R6::R6Class(

  classname = "ConceptLattice",

  public = list(

    initialize = function(extents, intents,
                          objects, attributes,
                          I = NULL) {

      private$objects <- objects
      private$attributes <- attributes
      private$extents <- extents
      private$intents <- intents

      if (!is.null(extents)) {

        private$concepts <- lapply(seq(ncol(intents)),
                                   function(i) {
                                     .concept_to_SparseSet(list(extents[, i], intents[, i]),
                                                           objects = objects,
                                                           attributes = attributes)
                                   })

        private$I <- I
        private$subconcept_matrix <- .subset(extents)

      }

    },

    size = function() {

      if (self$is_empty()) {

        return(0)

      }

      return(length(private$concepts))

    },

    is_empty = function() {

      return(is.null(private$concepts))

    },

    get_extents = function() {

      return(private$extents)

    },

    get_intents = function() {

      return(private$intents)

    },

    #' @description
    #' Plot the concept lattice
    #'
    #' @param object_names  (logical) If \code{TRUE}, plot object names, otherwise omit them from the diagram.
    #'
    #' @return Nothing, just plots the graph of the concept lattice.
    #' @export
    #'
    #' @importFrom hasseDiagram hasse
    plot = function(object_names = TRUE) {

      if (object_names) {

        labels <- sapply(private$concepts,
                         function(l) .concept_to_string(l,
                                                        private$objects,
                                                        private$attributes))

      } else {

        labels <- sapply(private$concepts,
                         function(l) {
                           v <- l$get_intent()
                           .set_to_string(S = v$get_vector(),
                                          attributes = v$get_attributes())
                         })

      }

      hasse(data = as.matrix(t(private$subconcept_matrix)),
            labels = labels,
            parameters = list(arrows = "backward"))

    },

    print = function() {

      if (self$is_empty()) {

        cat("An empty set of concepts.\n")

      } else {

        n <- length(private$concepts)

        cat("A set of", n, "concepts:\n")

        str <- sapply(seq(n), function(i) {

          conc <- private$concepts[[i]]

          paste0(i, ": ",
                 .concept_to_string(conc,
                                    objects = private$objects,
                                    attributes = private$attributes))

        })

        cat(str, sep = "\n")

      }

    },

    to_latex = function(ncols = 1,
                        numbered = TRUE,
                        align = TRUE) {

      if (!self$is_empty()) {

        concepts_to_latex(private$concepts,
                          ncols = ncols,
                          align = align,
                          numbered = numbered)

      }

    },

    get_concepts_by_id = function(indices) {

      if (!self$is_empty()) {

        indices <- indices[indices <= length(private$concepts)]
        return(private$concepts[indices])

      }

      return(list())

    },

    get_sublattice = function(...) {

      idx <- private$to_indices(...)

      if (length(idx) > 0) {

        idx <- .get_sublattice(private$subconcept_matrix,
                               starting_idx = idx)

        if (length(idx) > 1) {

          my_intents <- private$intents[, idx]
          my_extents <- private$extents[, idx]

        } else {

          my_intents <- .extract_column(private$intents, idx)
          my_extents <- .extract_column(private$extents, idx)

        }

        cl <- ConceptLattice$new(extents = my_extents,
                                 intents = my_intents,
                                 objects = private$objects,
                                 attributes = private$attributes,
                                 I = private$I)

        return(cl)

      }

    },

    #' @importFrom Matrix colSums
    join_irreducibles = function() {

      M <- .reduce_transitivity(private$subconcept_matrix)

      idx <- which(colSums(M) == 1)
      self$get_concepts_by_id(idx)

    },

    #' @importFrom Matrix colSums
    meet_irreducibles = function() {

      M <- .reduce_transitivity(t(private$subconcept_matrix))

      idx <- which(colSums(M) == 1)
      self$get_concepts_by_id(idx)

    },

    supremum = function(...) {

      idx <- private$to_indices(...)

      # Get the index of all superconcepts
      M <- private$subconcept_matrix[idx, ]
      candidates <- which(colSums(M) == length(idx))

      if (length(candidates) > 1) {

        # If more than one, obtain the minimum of
        # them:
        M2 <- t(private$subconcept_matrix)[candidates, candidates]

        candidates <- candidates[which(colSums(M2) == length(candidates))]

      }

      return(self$get_concepts_by_id(candidates))

    },

    infimum = function(...) {

      idx <- private$to_indices(...)

      # Obtain the index of all subconcepts
      M <- t(private$subconcept_matrix)[idx, ]
      candidates <- which(colSums(M) == length(idx))

      if (length(candidates) > 1) {

        # If more than one, get their maximum
        M2 <- private$subconcept_matrix[candidates, candidates]

        candidates <- candidates[which(colSums(M2) == length(candidates))]

      }

      return(self$get_concepts_by_id(candidates))

    },

    get_subconcepts = function(C) {

      idx <- private$to_indices(C)

      # Get the index of all subconcepts
      M <- t(private$subconcept_matrix)[idx, ]
      candidates <- which(M > 0)

      self$get_concepts_by_id(candidates)

    },

    get_superconcepts = function(C) {

      idx <- private$to_indices(C)

      # Get the index of all superconcepts
      M <- private$subconcept_matrix[idx, ]
      candidates <- which(M > 0)

      self$get_concepts_by_id(candidates)

    },

    #' @description
    #' Get support of each concept
    #'
    #' @return A vector with the support of each concept.
    #' @export
    compute_support = function() {

      if (!is.null(private$concept_support)) {

        return(private$concept_support)

      }

      my_I <- private$I
      my_I@x <- as.numeric(my_I@x)

      subsets <- .subset(private$intents, my_I)

      private$concept_support <- rowMeans(subsets)

      return(private$concept_support)

    }

  ),

  private = list(

    concepts = NULL,
    extents = NULL,
    intents = NULL,
    objects = NULL,
    attributes = NULL,
    subconcept_matrix = NULL,
    I = NULL,
    concept_support = NULL,

    concept_list_to_indices = function(concept_list) {

      extents <- lapply(concept_list,
                        function(l) l$get_extent()$get_vector())

      extents <- Reduce(cbind, extents)

      indices <- .equal_sets(x = extents,
                             y = private$extents)

      indices <- arrayInd(which(indices),
                          .dim = dim(indices))[, 2]

      return(indices)

    },

    to_indices = function(...) {

      dots <- list(...)
      if (is.list(dots[[1]])) {

        dots <- unlist(dots)

      }

      sets <- sapply(dots,
                     function(l)
                       inherits(l, "SparseConcept"))
      sets <- dots[which(sets)]

      indices <- sapply(dots, is.numeric)
      idx <- c()

      if (length(indices) > 0) {

        idx <- c(idx, Reduce(c, dots[indices]))

      }

      if (length(sets) > 0) {

        idx <- c(idx,
                 private$concept_list_to_indices(sets))

      }

      return(idx)

    }

  )

)
