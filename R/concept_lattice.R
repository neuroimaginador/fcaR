#' @title
#' R6 class for a context lattice
#'
#' @description
#' This class implements the data structure and methods for concept lattices.
#'
#' @examples
#' # Build a formal context
#' fc_planets <- FormalContext$new(planets)
#'
#' # Find the concepts
#' fc_planets$find_concepts()
#'
#' # Find join- and meet- irreducible elements
#' fc_planets$concepts$join_irreducibles()
#' fc_planets$concepts$meet_irreducibles()
#'
#' # Get concept support
#' fc_planets$concepts$support()
#'
#' @export
#' @import R6
#'
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

      if (!capabilities()["long.double"]) {

        private$can_plot <- FALSE

      }

      # Create the SparseConcepts
      if (!is.null(extents)) {

        private$concepts <- .matrix_to_concepts(
          M_ext = extents,
          M_int = intents,
          objects = objects,
          attributes = attributes)

        private$I <- I

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
    #' @param to_latex      (logical) If \code{TRUE}, export the plot as a \code{tikzpicture} environment that can be included in a \code{LaTeX} file.
    #' @param ...          Other parameters to be passed to the \code{tikzDevice} that renders the lattice in \code{LaTeX}, or for the figure caption. See \code{Details}.
    #'
    #' @details
    #' Particular parameters that control the size of the \code{tikz} output are: \code{width}, \code{height} (both in inches), and \code{pointsize} (in points), that should be set to the font size used in the \code{documentclass} header in the \code{LaTeX} file where the code is to be inserted.
    #'
    #' If a \code{caption} is provided, the whole \code{tikz} picture will be wrapped by a \code{figure} environment and the caption set.
    #'
    #' @return If \code{to_latex} is \code{FALSE}, it returns nothing, just plots the graph of the concept lattice. Otherwise, this function returns the \code{LaTeX} code to reproduce the concept lattice.
    #' @export
    #'
    plot = function(object_names = TRUE,
                    to_latex = FALSE,
                    ...) {

      if (self$size() == 0) {

        warning("No concepts.", call. = FALSE)
        return(invisible(NULL))

      }

      if (!private$can_plot) {

        warning("The R system has not the needed capabilities to plot.",
                call. = FALSE)
        return(invisible(FALSE))

      }

      if (!requireNamespace("hasseDiagram", quietly = TRUE)) {

        warning("You have not installed the 'hasseDiagram' package, which is needed to plot the lattice.",
                call. = FALSE)

        return(invisible(FALSE))

      }

      if ((self$size() > 0) & (is.null(private$subconcept_matrix))) {

        private$subconcept_matrix <- subsetSpM(private$pr_extents)

      }

      # print(private$subconcept_matrix)

      lattice_plot(private$concepts,
                   private$subconcept_matrix,
                   private$objects,
                   private$attributes,
                   object_names,
                   to_latex, ...)


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
    #' @param print (logical) Print to output?
    #' @param ncols (integer) Number of columns of the output.
    #' @param numbered (logical) Number the concepts?
    #' @param align  (logical) Align objects and attributes independently?
    #'
    #' @return
    #' The \code{LaTeX} code to list all concepts.
    #'
    #' @export
    to_latex = function(print = TRUE,
                        ncols = 1,
                        numbered = TRUE,
                        align = TRUE) {

      if (!self$is_empty()) {

        output <- concepts_to_latex(private$concepts,
                                    ncols = ncols,
                                    align = align,
                                    numbered = numbered)

        if (print) {

          cat(output)

        }

        return(invisible(output))

      }

    },

    #' @description
    #' Get Concepts by Index
    #'
    #' @param indices (numeric or logical vector) The indices of the concepts to return as a list of SparseConcepts. It can be a vector of logicals where \code{TRUE} elements are to be retained.
    #'
    #' @return A list of SparseConcepts.
    #'
    #' @export
    `[` = function(indices) {

      if (!self$is_empty()) {

        if (is.logical(indices)) {

          indices <- which(indices)

        }

        indices <- indices[indices <= length(private$concepts)]

        elements <- private$concepts[indices]
        class(elements) <- "conceptlist"
        return(elements)

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

        if ((self$size() > 0) & (is.null(private$subconcept_matrix))) {

          private$subconcept_matrix <- subsetSpM(private$pr_extents) %>% tSpM()

        }

        idx <- .get_sublattice(private$subconcept_matrix,
                               starting_idx = idx)

        my_intents <- private$pr_intents %>%
          extract_columns(idx)
        my_extents <- private$pr_extents %>%
          extract_columns(idx)

        cl <- ConceptLattice$new(extents = my_extents,
                                 intents = my_intents,
                                 objects = private$objects,
                                 attributes = private$attributes,
                                 I = private$I)

        return(cl)

      }

    },

    #' @description
    #' Meet-irreducible Elements
    #'
    #' @return
    #' The meet-irreducible elements in the concept lattice.
    #'
    #' @export
    #'
    meet_irreducibles= function() {

      if ((self$size() > 0) & (is.null(private$subconcept_matrix))) {

        private$subconcept_matrix <- subsetSpM(private$pr_extents)

      }

      if (is.null(private$reduced_matrix)) {

        private$reduced_matrix <- .reduce_transitivity(private$subconcept_matrix)

      }

      idx <- which(colSums(private$reduced_matrix) == 1)
      self[idx]

    },

    #' @description
    #' Join-irreducible Elements
    #'
    #' @return
    #' The join-irreducible elements in the concept lattice.
    #'
    #' @export
    #'
    join_irreducibles= function() {

      if ((self$size() > 0) & (is.null(private$subconcept_matrix))) {

        private$subconcept_matrix <- subsetSpM(private$pr_extents)

      }

      M <- .reduce_transitivity(private$subconcept_matrix)

      idx <- which(rowSums(M) == 1)
      self[idx]

    },

    #' @description
    #' Decompose a concept as the supremum of meet-irreducible concepts
    #'
    #' @param C A list of \code{SparseConcept}s
    #' @return
    #' A list, each field is the set of meet-irreducible elements whose supremum is the corresponding element in \code{C}.
    #'
    #' @export
    #'
    decompose = function(C) {

      irreducible <- self$meet_irreducibles()
      irr_intents <- do.call(cbindSpM,
                             lapply(irreducible,
                                    function(r) r$get_intent()$get_vector()))

      ss <- lapply(C,
                   function(r) {

                     if (r$get_intent()$cardinal() == 0) return(r)

                     id <- whichSpM(subsetSpM(irr_intents,
                                                 r$get_intent()$get_vector()))

                     if (length(id) > 1) {

                       MM <- subsetSpM(irr_intents %>% extract_columns(id))
                       id <- id[rowSums(MM) == 1]

                     }
                     foo <- irreducible[id]

                     class(foo) <- "conceptlist"

                     return(foo)

                   })


      return(ss)

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

      if ((self$size() > 0) & (is.null(private$subconcept_matrix))) {

        private$subconcept_matrix <- subsetSpM(private$pr_extents)

      }

      return(self[join(private$subconcept_matrix, idx)])

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

      if ((self$size() > 0) & (is.null(private$subconcept_matrix))) {

        private$subconcept_matrix <- subsetSpM(private$pr_extents)

      }

      return(self[meet(private$subconcept_matrix, idx)])

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

      if ((self$size() > 0) & (is.null(private$subconcept_matrix))) {

        private$subconcept_matrix <- subsetSpM(private$pr_extents) %>% tSpM()

      }

      # Get the index of all subconcepts
      M <- private$subconcept_matrix %>%
        tSpM() %>%
        extract_rows(idx) %>%
        colSums()
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

      if ((self$size() > 0) & (is.null(private$subconcept_matrix))) {

        private$subconcept_matrix <- subsetSpM(private$pr_extents) %>% tSpM()

      }

      # Get the index of all superconcepts
      M <- private$subconcept_matrix %>%
        extract_rows(idx) %>%
        colSums()
      candidates <- which(M > 0)

      self[candidates]

    },

    #' @description
    #' Lower Neighbours of a Concept
    #'
    #' @param C (\code{SparseConcept}) The concept to which find its lower neighbours
    #'
    #' @return
    #' A list with the lower neighbours of \code{C}.
    #'
    #' @export
    lower_neighbours = function(C) {

      idx <- private$to_indices(C)

      if ((self$size() > 0) & (is.null(private$subconcept_matrix))) {

        private$subconcept_matrix <- subsetSpM(private$pr_extents)

      }

      if (is.null(private$reduced_matrix)) {

        private$reduced_matrix <- .reduce_transitivity(private$subconcept_matrix)

      }

      self[(private$reduced_matrix %>% extract_rows(idx) %>% colSums()) > 0]

    },

    #' @description
    #' Upper Neighbours of a Concept
    #'
    #' @param C (\code{SparseConcept}) The concept to which find its upper neighbours
    #'
    #' @return
    #' A list with the upper neighbours of \code{C}.
    #'
    #' @export
    upper_neighbours = function(C) {

      idx <- private$to_indices(C)

      if ((self$size() > 0) & (is.null(private$subconcept_matrix))) {

        private$subconcept_matrix <- subsetSpM(private$pr_extents)

      }

      if (is.null(private$reduced_matrix)) {

        private$reduced_matrix <- .reduce_transitivity(private$subconcept_matrix)

      }

      M <- private$reduced_matrix %>%
        extract_columns(idx) %>%
        rowSums()

      self[which(M > 0)]

    },

    #' @description
    #' Get support of each concept
    #'
    #' @return A vector with the support of each concept.
    #' @export
    support = function() {

      if (self$size() == 0) {

        return(numeric(0))

      }

      if (!is.null(private$concept_support)) {

        return(private$concept_support)

      }

      my_I <- private$I
      # my_I$px <- as.numeric(my_I$px)

      subsets <- subsetSpM(private$pr_intents, my_I)

      private$concept_support <- colSums(subsets) / nrow.SpM(subsets)

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
    reduced_matrix = NULL,
    I = NULL,
    concept_support = NULL,

    can_plot = TRUE,

    concept_list_to_indices = function(concept_list) {

      extents <- lapply(concept_list,
                        function(l) l$get_extent()$get_vector())

      extents <- Reduce(cbindSpM, extents)

      indices <- equalSpM(x = extents,
                          y = private$pr_extents)

      indices <- arrayInd(whichSpM(indices),
                          .dim = dim.SpM(indices))[, 1]

      return(indices)

    },

    to_indices = function(...) {

      dots <- list(...)
      if (is.list(dots[[1]])) {

        dots <- unlist(dots)

      }

      if (is.logical(dots[[1]])) {

        return(which(dots[[1]]))

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
