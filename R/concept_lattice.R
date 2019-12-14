#' @title
#' R6 class for a context lattice
#'
#' @description
#' This class implements the data structure and methods for concept lattices.
#'
#' @export
ConceptLattice <- R6::R6Class(

  classname = "ConceptLattice",

  public = list(

    #' @description
    #' Create a new \code{ConceptLattice} object.
    #'
    #' @param extents (\code{dgCMatrix}) The extents of all concepts
    #' @param intents (\code{dgCMatrix}) The intents of all concepts
    #' @param objects (character vector) Names of the objects in the formal context
    #' @param attributes (character vector) Names of the attributes in the formal context
    #' @param I (\code{dgCMatrix}) The matrix of the formal context
    #'
    #' @return
    #' A new \code{ConceptLattice} object.
    #'
    #' @export
    initialize = function(extents, intents,
                          objects, attributes,
                          I = NULL) {

      private$objects <- objects
      private$attributes <- attributes
      private$pr_extents <- extents
      private$pr_intents <- intents

      # Create the SparseConcepts
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

    #' @description
    #' Size of the Lattice
    #'
    #' @return
    #' The number of concepts in the lattice.
    #'
    #' @export
    size = function() {

      if (self$is_empty()) {

        return(0)

      }

      return(length(private$concepts))

    },

    #' @description
    #' Is the lattice empty?
    #'
    #' @return
    #' \code{TRUE} if the lattice has no concepts.
    #' @export
    is_empty = function() {

      return(is.null(private$concepts))

    },

    #' @description
    #' Concept Extents
    #'
    #' @return
    #' The extents of all concepts, as a \code{dgCMatrix}.
    #'
    #' @export
    extents = function() {

      return(private$pr_extents)

    },

    #' @description
    #' Concept Intents
    #'
    #' @return
    #' The intents of all concepts, as a \code{dgCMatrix}.
    #'
    #' @export
    intents = function() {

      return(private$pr_intents)

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

    #' @description
    #' Print the Concept Lattice
    #'
    #' @return
    #' Nothing, just prints the lattice.
    #'
    #' @export
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

    #' @description
    #' Write in LaTeX
    #'
    #' @param ncols (integer) Number of columns of the output.
    #' @param numbered (logical) Number the concepts?
    #' @param align  (logical) Align objects and attributes independently?
    #'
    #' @return
    #' The \code{LaTeX} code to list all concepts.
    #'
    #' @export
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

    #' @description
    #' Get Concepts by Index
    #'
    #' @param indices (numeric vector) The indices of the concepts to return as a list of SparseConcepts.
    #'
    #' @return A list of SparseConcepts.
    #'
    #' @export
    `[` = function(indices) {

      if (!self$is_empty()) {

        indices <- indices[indices <= length(private$concepts)]
        return(private$concepts[indices])

      }

      return(list())

    },

    #' @description
    #' Sublattice
    #'
    #' @param ... See Details.
    #'
    #' @details
    #' As argument, one can provide both integer indices or \code{SparseConcepts}, separated by commas. The corresponding concepts are used to generate a sublattice.
    #'
    #' @return
    #' The generated sublattice as a new \code{ConceptLattice} object.
    #'
    #' @export
    sublattice = function(...) {

      idx <- private$to_indices(...)

      if (length(idx) > 0) {

        idx <- .get_sublattice(private$subconcept_matrix,
                               starting_idx = idx)

        if (length(idx) > 1) {

          my_intents <- private$pr_intents[, idx]
          my_extents <- private$pr_extents[, idx]

        } else {

          my_intents <- .extract_column(private$pr_intents, idx)
          my_extents <- .extract_column(private$pr_extents, idx)

        }

        cl <- ConceptLattice$new(extents = my_extents,
                                 intents = my_intents,
                                 objects = private$objects,
                                 attributes = private$attributes,
                                 I = private$I)

        return(cl)

      }

    },


    #' @description
    #' Join-irreducible Elements
    #'
    #' @return
    #' The join-irreducible elements in the concept lattice.
    #'
    #' @export
    #'
    #' @importFrom Matrix colSums
    join_irreducibles = function() {

      M <- .reduce_transitivity(private$subconcept_matrix)

      idx <- which(colSums(M) == 1)
      self[idx]

    },

    #' @description
    #' Meet-irreducible Elements
    #'
    #' @return
    #' The meet-irreducible elements in the concept lattice.
    #'
    #' @export
    #'
    #' @importFrom Matrix colSums
    meet_irreducibles = function() {

      M <- .reduce_transitivity(t(private$subconcept_matrix))

      idx <- which(colSums(M) == 1)
      self[idx]

    },

    #' @description
    #' Supremum of Concepts
    #'
    #' @param ... See Details.
    #'
    #' @details
    #' As argument, one can provide both integer indices or \code{SparseConcepts}, separated by commas. The corresponding concepts are used to compute their supremum in the lattice.
    #'
    #' @return
    #' The supremum of the list of concepts.
    #'
    #' @export
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

      return(self[candidates])

    },

    #' @description
    #' Infimum of Concepts
    #'
    #' @param ... See Details.
    #'
    #' @details
    #' As argument, one can provide both integer indices or \code{SparseConcepts}, separated by commas. The corresponding concepts are used to compute their infimum in the lattice.
    #'
    #' @return
    #' The infimum of the list of concepts.
    #'
    #' @export
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

      return(self[candidates])

    },

    #' @description
    #' Subconcepts of a Concept
    #'
    #' @param C (numeric or \code{SparseConcept}) The concept to which determine all its subconcepts.
    #'
    #' @return
    #' A list with the subconcepts.
    #' @export
    subconcepts = function(C) {

      idx <- private$to_indices(C)

      # Get the index of all subconcepts
      M <- t(private$subconcept_matrix)[idx, ]
      candidates <- which(M > 0)

      self[candidates]

    },

    #' @description
    #' Superconcepts of a Concept
    #'
    #' @param C (numeric or \code{SparseConcept}) The concept to which determine all its superconcepts.
    #'
    #' @return
    #' A list with the superconcepts.
    #' @export
    superconcepts = function(C) {

      idx <- private$to_indices(C)

      # Get the index of all superconcepts
      M <- private$subconcept_matrix[idx, ]
      candidates <- which(M > 0)

      self[candidates]

    },

    #' @description
    #' Get support of each concept
    #'
    #' @return A vector with the support of each concept.
    #' @export
    support = function() {

      if (!is.null(private$concept_support)) {

        return(private$concept_support)

      }

      my_I <- private$I
      my_I@x <- as.numeric(my_I@x)

      subsets <- .subset(private$pr_intents, my_I)

      private$concept_support <- rowMeans(subsets)

      return(private$concept_support)

    }

  ),

  private = list(

    concepts = NULL,
    pr_extents = NULL,
    pr_intents = NULL,
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
                             y = private$pr_extents)

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
