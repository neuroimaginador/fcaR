#' @title
#' R6 class for a concept lattice
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
  inherit = ConceptSet,
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

      if (!capabilities()["long.double"] & getRversion() < "4.1.0") {
        private$can_plot <- FALSE
      }

      super$initialize(
        extents, intents,
        objects, attributes,
        I
      )
    },

    #' @description
    #' Plot the concept lattice
    #'
    #' @param object_names (logical) Deprecated. Use \code{mode} instead. If \code{TRUE} (default), implies \code{mode = "reduced"} or similar depending on heuristics. Kept for backward compatibility.
    #' @param to_latex     (logical) If \code{TRUE}, exports the plot as TikZ code (LaTeX) instead of drawing it. Returns an object of class \code{tikz_code} that prints the LaTeX code to console.
    #' @param method       (character) The layout algorithm to use. Options are:
    #' \itemize{
    #'   \item \code{"sugiyama"} (default): A hierarchical layout that minimizes edge crossings and centers nodes (similar to ConExp or hasseDiagram).
    #'   \item \code{"force"}: A force-directed (spring) layout, useful for large or non-hierarchical lattices.
    #' }
    #' @param mode         (character) The labeling mode for the nodes. If \code{NULL} (default), a heuristic based on lattice size is used. Options are:
    #' \itemize{
    #'   \item \code{"reduced"}: Standard FCA labeling. Nodes are labeled with an attribute (or object) only if they are the supreme (or infimum) of that attribute (or object).
    #'   \item \code{"full"}: Each node shows its complete extent and intent.
    #'   \item \code{"attributes"}: Nodes show only their intent (attributes).
    #'   \item \code{"empty"}: Nodes are drawn as points without labels. Recommended for very large lattices (>50 concepts).
    #' }
    #' @param ...          Other parameters passed to the internal plotting function (e.g., graphical parameters for \code{ggraph}).
    #'
    #' @return If \code{to_latex} is \code{FALSE}, it returns (invisibly) the \code{ggplot2} object representing the graph.
    #' If \code{to_latex} is \code{TRUE}, it returns a \code{tikz_code} object containing the LaTeX code.
    #' @export
    plot = function(object_names = TRUE, to_latex = FALSE, method = c("sugiyama", "force"), mode = NULL, ...) {
      # 1. Verificación de estado
      if (self$size() == 0) {
        warning("No concepts to plot.", call. = FALSE)
        return(invisible(NULL))
      }

      if (is.null(private$subconcept_matrix)) {
        private$subconcept_matrix <- as(.subset(private$pr_extents), "nMatrix")
        private$covering_matrix <- .reduce_transitivity(private$subconcept_matrix)

        # stop("Lattice structure not computed. Cannot plot.")
      }

      # 2. Construcción del índice de nodos (Data Frame mínimo)
      # Solo creamos los IDs (1..N). La función externa calculará grados y layout.
      nodes_df <- data.frame(
        id = seq_len(self$size()),
        stringsAsFactors = FALSE
      )

      # --- CONVERSIÓN IMPORTANTE ---
      # Convertimos los SparseSets a listas estándar de R para que la función de plot
      # pueda hacer `extents[[i]]` sin errores S4.
      extents_list <- sparse_to_list(private$pr_extents)
      intents_list <- sparse_to_list(private$pr_intents)

      # 3. Delegación a lattice_plot
      # Pasamos los datos privados necesarios (matriz, objetos, atributos)
      method <- match.arg(method)
      lattice_plot(
        nodes_df = nodes_df,
        cover_matrix = private$covering_matrix,
        method = method, # Pasamos el método
        mode = mode, # Pasamos el modo (o NULL para heurística)

        objects = private$objects,
        attributes = private$attributes,

        # Pasamos los conjuntos dispersos para calcular reduced/full labels
        extents = extents_list,
        intents = intents_list,
        object_names = self$objects,
        to_latex = to_latex,
        ...
      )
    },

    #' @description
    #' Sublattice
    #'
    #' @param ... See Details.
    #'
    #' @details
    #' As argument, one can provide both integer indices or \code{Concepts}, separated by commas. The corresponding concepts are used to generate a sublattice.
    #'
    #' @return
    #' The generated sublattice as a new \code{ConceptLattice} object.
    #'
    #' @export
    sublattice = function(...) {
      idx <- private$to_indices(...)

      if (length(idx) > 0) {
        if ((self$size() > 0) & (is.null(private$subconcept_matrix))) {
          private$subconcept_matrix <- .subset(private$pr_extents)
        }

        idx <- .get_sublattice(private$subconcept_matrix,
          starting_idx = idx
        )

        if (length(idx) > 1) {
          my_intents <- private$pr_intents[, idx]
          my_extents <- private$pr_extents[, idx]
        } else {
          my_intents <- .extract_column(private$pr_intents, idx)
          my_extents <- .extract_column(private$pr_extents, idx)
        }

        cl <- ConceptLattice$new(
          extents = my_extents,
          intents = my_intents,
          objects = private$objects,
          attributes = private$attributes,
          I = private$I
        )

        return(cl)
      }
    },

    #' @description Top of a Lattice
    #'
    #' @return The top of the Concept Lattice
    #' @export
    #'
    #' @examples
    #' fc <- FormalContext$new(planets)
    #' fc$find_concepts()
    #' fc$concepts$top()
    #'
    top = function() {
      if ((self$size() > 0) & (is.null(private$subconcept_matrix))) {
        private$subconcept_matrix <- .subset(private$pr_extents)
      }

      idx <- which(Matrix::colSums(private$subconcept_matrix) == self$size())

      self[idx]$to_list()[[1]]
    },

    #' @description Bottom of a Lattice
    #'
    #' @return The bottom of the Concept Lattice
    #' @export
    #'
    #' @examples
    #' fc <- FormalContext$new(planets)
    #' fc$find_concepts()
    #' fc$concepts$bottom()
    #'
    bottom = function() {
      if ((self$size() > 0) & (is.null(private$subconcept_matrix))) {
        private$subconcept_matrix <- .subset(private$pr_extents)
      }

      idx <- which(Matrix::colSums(private$subconcept_matrix) == 1)

      self[idx]$to_list()[[1]]
    },


    #' @description
    #' Join-irreducible Elements
    #'
    #' @return
    #' The join-irreducible elements in the concept lattice.
    #'
    #' @export
    #'
    join_irreducibles = function() {
      if ((self$size() > 0) & (is.null(private$subconcept_matrix))) {
        private$subconcept_matrix <- .subset(private$pr_extents)
      }

      if (is.null(private$covering_matrix)) {
        private$covering_matrix <- .reduce_transitivity(private$subconcept_matrix)
      }

      idx <- which(Matrix::colSums(private$covering_matrix) == 1)
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
    meet_irreducibles = function() {
      if ((self$size() > 0) & (is.null(private$subconcept_matrix))) {
        private$subconcept_matrix <- .subset(private$pr_extents)
      }

      M <- .reduce_transitivity(Matrix::t(private$subconcept_matrix))

      idx <- which(Matrix::colSums(M) == 1)
      self[idx]
    },

    #' @description
    #' Decompose a concept as the supremum of meet-irreducible concepts
    #'
    #' @param C A list of \code{Concept}s
    #' @return
    #' A list, each field is the set of meet-irreducible elements whose supremum is the corresponding element in \code{C}.
    #'
    #' @export
    #'
    decompose = function(C) {
      irreducible <- self$meet_irreducibles()

      irr_intents <- irreducible$intents()

      C_intents <- C$intents()

      ss <- lapply(
        seq(C$size()),
        function(i) {
          r <- Matrix::Matrix(C_intents[, i],
            sparse = TRUE
          )

          if (sum(r) == 0) {
            return(C[i])
          }

          id <- Matrix::which(.subset(
            irr_intents,
            r
          ))

          if (length(id) > 1) {
            MM <- .subset(irr_intents[, id])
            id <- id[Matrix::rowSums(MM) == 1]
          }
          decomposition <- irreducible[id]

          return(decomposition)
        }
      )


      return(ss)
    },

    #' @description
    #' Supremum of Concepts
    #'
    #' @param ... See Details.
    #'
    #' @details
    #' As argument, one can provide both integer indices or \code{Concepts}, separated by commas. The corresponding concepts are used to compute their supremum in the lattice.
    #'
    #' @return
    #' The supremum of the list of concepts.
    #'
    #' @export
    supremum = function(...) {
      idx <- private$to_indices(...)

      if ((self$size() > 0) & (is.null(private$subconcept_matrix))) {
        private$subconcept_matrix <- .subset(private$pr_extents)
      }

      return(self[join(private$subconcept_matrix, idx)]$to_list()[[1]])
    },

    #' @description
    #' Infimum of Concepts
    #'
    #' @param ... See Details.
    #'
    #' @details
    #' As argument, one can provide both integer indices or \code{Concepts}, separated by commas. The corresponding concepts are used to compute their infimum in the lattice.
    #'
    #' @return
    #' The infimum of the list of concepts.
    #'
    #' @export
    infimum = function(...) {
      idx <- private$to_indices(...)

      if ((self$size() > 0) & (is.null(private$subconcept_matrix))) {
        private$subconcept_matrix <- .subset(private$pr_extents)
      }

      return(self[meet(private$subconcept_matrix, idx)]$to_list()[[1]])
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
        private$subconcept_matrix <- .subset(private$pr_extents)
      }

      # Get the index of all subconcepts
      M <- Matrix::t(private$subconcept_matrix)[idx, ]
      candidates <- Matrix::which(M > 0)

      if (length(candidates) > 1) {
        return(
          ConceptLattice$new(
            attributes = private$attributes,
            objects = private$objects,
            extents = private$pr_extents[, candidates],
            intents = private$pr_intents[, candidates],
            I = private$I
          )
        )
      }

      return(
        ConceptLattice$new(
          attributes = private$attributes,
          objects = private$objects,
          extents = .extract_column(
            private$pr_extents,
            candidates
          ),
          intents = .extract_column(
            private$pr_intents,
            candidates
          ),
          I = private$I
        )
      )
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
        private$subconcept_matrix <- .subset(private$pr_extents)
      }

      # Get the index of all superconcepts
      M <- private$subconcept_matrix[idx, ]
      candidates <- which(M > 0)

      if (length(candidates) > 1) {
        return(
          ConceptLattice$new(
            attributes = private$attributes,
            objects = private$objects,
            extents = private$pr_extents[, candidates],
            intents = private$pr_intents[, candidates],
            I = private$I
          )
        )
      }

      return(
        ConceptLattice$new(
          attributes = private$attributes,
          objects = private$objects,
          extents = .extract_column(
            private$pr_extents,
            candidates
          ),
          intents = .extract_column(
            private$pr_intents,
            candidates
          ),
          I = private$I
        )
      )
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
        private$subconcept_matrix <- .subset(private$pr_extents)
      }

      if (is.null(private$covering_matrix)) {
        private$covering_matrix <- .reduce_transitivity(private$subconcept_matrix)
      }

      self[which(private$covering_matrix[, idx] > 0)]
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
        private$subconcept_matrix <- .subset(private$pr_extents)
      }

      if (is.null(private$covering_matrix)) {
        private$covering_matrix <- .reduce_transitivity(private$subconcept_matrix)
      }

      self[which(private$covering_matrix[idx, ] > 0)]
    }
  ),
  private = list(
    subconcept_matrix = NULL,
    covering_matrix = NULL,
    can_plot = TRUE,
    concept_list_to_indices = function(concept_list) {
      extents <- lapply(
        concept_list,
        function(l) l$get_extent()$get_vector()
      )

      extents <- Reduce(cbind, extents)

      indices <- .equal_sets(
        x = extents,
        y = private$pr_extents
      )

      indices <- arrayInd(Matrix::which(indices),
        .dim = dim(indices)
      )[, 2]

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

      csets_idx <- sapply(
        dots,
        function(l) inherits(l, "ConceptSet")
      )
      if (length(csets_idx) > 0) {
        csets <- sapply(
          dots[csets_idx],
          function(l) l$to_list()
        ) %>%
          unlist()
      } else {
        csets <- c()
      }

      sets <- sapply(
        dots,
        function(l) {
          inherits(l, "Concept")
        }
      )
      sets <- c(csets, dots[which(sets)])

      indices <- sapply(dots, is.numeric)
      idx <- c()

      if (length(indices) > 0) {
        idx <- c(idx, Reduce(c, dots[indices]))
      }

      if (length(sets) > 0) {
        idx <- c(
          idx,
          private$concept_list_to_indices(sets)
        )
      }

      return(idx)
    }
  )
)
