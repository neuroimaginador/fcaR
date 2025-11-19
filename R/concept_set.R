#' @title
#' R6 class for a set of concepts
#'
#' @description
#' This class implements the data structure and methods for concept sets.
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
#' @export
#' @import R6
#'
ConceptSet <- R6::R6Class(
  classname = "ConceptSet",
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

      private$I <- I
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

      return(ncol(private$pr_extents))
    },

    #' @description
    #' Is the lattice empty?
    #'
    #' @return
    #' \code{TRUE} if the lattice has no concepts.
    #' @export
    is_empty = function() {
      return(is.null(private$pr_extents))
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
    #' Print the Concept Set
    #'
    #' @return
    #' Nothing, just prints the concepts
    #'
    #' @export
    print = function() {
      if (self$is_empty()) {
        cat("An empty set of concepts.\n")
        return(invisible(self))
      }

      # Obtenemos dimensiones
      # Asumimos que private$pr_extents es dgCMatrix
      n <- ncol(private$pr_extents)

      # --- LÓGICA DE TRUNCAMIENTO (Opcional pero recomendada) ---
      # Si tienes 10,000 conceptos, imprimir todos colgará la consola aunque el cálculo sea rápido.
      limit <- 50 # O leer de options()
      should_truncate <- n > limit

      n_to_print <- if (should_truncate) limit else n

      cat("A set of", n, "concepts:\n")

      # Llamada a C++ optimizada
      # Pasamos las matrices S4 enteras directamente
      # NOTA: Asegúrate de pasar solo las columnas que quieres imprimir si vas a truncar,
      # o modifica el C++ para aceptar un rango start/end.
      # Para máxima velocidad en "print all", pasamos todo.

      decimal_places <- fcaR_options("decimal_places")

      # Generamos TODAS las strings (es muy rápido en C++ incluso para 100k conceptos)
      # Si la memoria es problema, habría que pasar subsets al C++, pero raramente lo es para texto.
      all_strings <- get_concept_strings_cpp(
        private$pr_extents,
        private$pr_intents,
        private$objects,
        private$attributes,
        decimal_places
      )

      # Imprimir
      if (should_truncate) {
        cat(all_strings[1:limit], sep = "\n")
        cat(sprintf("... and %d more concepts.\n", n - limit))
      } else {
        cat(all_strings, sep = "\n")
      }

      invisible(self)
    },
    # print = function() {
    #   if (self$is_empty()) {
    #     cat("An empty set of concepts.\n")
    #   } else {
    #     n <- ncol(private$pr_extents)
    #
    #     cat("A set of", n, "concepts:\n")
    #
    #     str <- sapply(seq(n), function(i) {
    #       vA <- Matrix::Matrix(private$pr_extents[, i], sparse = TRUE)
    #       vB <- Matrix::Matrix(private$pr_intents[, i], sparse = TRUE)
    #
    #       paste0(
    #         i, ": ",
    #         .concept_to_string(vA, vB,
    #           objects = private$objects,
    #           attributes = private$attributes
    #         )
    #       )
    #     })
    #
    #     cat(str, sep = "\n")
    #   }
    # },

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
        output <- concepts_to_latex(private$pr_extents,
          private$pr_intents,
          private$objects,
          private$attributes,
          ncols = ncols,
          align = align,
          numbered = numbered
        )

        if (print) {
          cat(output)
        }

        return(invisible(output))
      }
    },

    #' @description
    #' Returns a list with all the concepts
    #'
    #' @return A list of concepts.
    #' @export
    to_list = function() {
      if (self$is_empty()) {
        return(list())
      }

      elements <- .matrix_to_concepts(
        M_ext = private$pr_extents,
        M_int = private$pr_intents,
        objects = private$objects,
        attributes = private$attributes
      )

      class(elements) <- c("list")

      return(elements)
    },

    #' @description
    #' Subsets a ConceptSet
    #'
    #' @param indices (numeric or logical vector) The indices of the concepts to return as a list of Concepts. It can be a vector of logicals where \code{TRUE} elements are to be retained.
    #'
    #' @return Another ConceptSet.
    #'
    #' @export
    `[` = function(indices) {
      if (!self$is_empty()) {
        if (is.logical(indices)) {
          indices <- which(indices)
        }

        indices <- indices[indices <= ncol(private$pr_extents)]

        return(ConceptSet$new(
          extents = Matrix::Matrix(private$pr_extents[, indices],
            sparse = TRUE
          ),
          intents = Matrix::Matrix(private$pr_intents[, indices],
            sparse = TRUE
          ),
          objects = private$objects,
          attributes = private$attributes,
          I = private$I
        ))
      }

      return(ConceptSet$new(
        extents = NULL,
        intents = NULL,
        objects = private$objects,
        attributes = private$attributes,
        I = private$I
      ))
    },

    #' @description
    #' Individual Concepts
    #'
    #' @param index (numeric) The index of the concept to return.
    #'
    #' @return The Concept.
    #'
    #' @export
    sub = function(index) {
      if (!self$is_empty()) {
        index <- index[index <= ncol(private$pr_extents)]

        if (length(index) > 0) {
          return(self[index]$to_list()[[1]])
        }
      }

      return(NULL)
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

      private$concept_support <- Matrix::colSums(private$pr_extents) / nrow(private$pr_extents)

      return(private$concept_support)
    },

    #' @description
    #' Compute the stability of each concept
    #'
    #' @return A numeric vector with the stability of each concept.
    #' @export
    stability = function() {
      # Pasamos directamente el objeto SparseSet (o su matriz interna)
      # El wrapper de R se encarga de la conversión a CsparseMatrix
      return(calculate_stability(private$pr_extents))
    }
  ),
  private = list(
    pr_extents = NULL,
    pr_intents = NULL,
    objects = NULL,
    attributes = NULL,
    I = NULL,
    concept_support = NULL
  )
)
