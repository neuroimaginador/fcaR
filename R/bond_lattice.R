#' @title
#' R6 class for a Bond Lattice
#'
#' @description
#' This class extends \code{ConceptLattice} to represent the lattice of bonds between
#' two formal contexts \eqn{K_1} and \eqn{K_2}.
#' It inherits all lattice operations (finding irreducible elements, subconcepts, etc.)
#' and provides specific methods to extract bonds as matrices and FormalContexts.
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' mat1 <- matrix(sample(0:1, 15, replace = TRUE), nrow = 5, ncol = 3)
#' rownames(mat1) <- paste0("O", 1:5)
#' colnames(mat1) <- paste0("A", 1:3)
#' fc1 <- FormalContext$new(mat1)
#'
#' mat2 <- matrix(sample(0:1, 12, replace = TRUE), nrow = 4, ncol = 3)
#' rownames(mat2) <- paste0("P", 1:4)
#' colnames(mat2) <- paste0("B", 1:3)
#' fc2 <- FormalContext$new(mat2)
#'
#' bl <- bonds(fc1, fc2)
#'
#' # Extract all bonds as FormalContext objects
#' my_bonds <- bl$get_bonds()
#' print(my_bonds[[1]])
#' }
#'
#' @export
#' @import R6
#'
BondLattice <- R6::R6Class(
  classname = "BondLattice",
  inherit = ConceptLattice,
  public = list(
    #' @field elapsed (numeric) Time elapsed during bond computation (C++ level)
    elapsed = NULL,

    #' @description
    #' Initialize a BondLattice object.
    #'
    #' @param extents (dgCMatrix) The extents of all concepts
    #' @param intents (dgCMatrix) The intents of all concepts
    #' @param objects (character) Names of the objects (not required for bonds, can be generic)
    #' @param attributes (character) Names of the attributes (flattened $G_1 \\times M_2$)
    #' @param I (matrix) Built incidence matrix
    #' @param fc1 (FormalContext) The first formal context
    #' @param fc2 (FormalContext) The second formal context
    #'
    #' @return A new \code{BondLattice} object.
    initialize = function(extents, intents, objects, attributes, I, fc1, fc2) {
      if (missing(fc1) || missing(fc2)) {
        stop("fc1 and fc2 must be provided.", call. = FALSE)
      }
      if (!inherits(fc1, "FormalContext") || !inherits(fc2, "FormalContext")) {
        stop("fc1 and fc2 must be FormalContext objects.", call. = FALSE)
      }

      super$initialize(
        extents = extents,
        intents = intents,
        objects = objects,
        attributes = attributes,
        I = I
      )

      private$fc1 <- fc1$clone(deep = TRUE)
      private$fc2 <- fc2$clone(deep = TRUE)
    },

    #' @description
    #' Extract the bonds represented by the intents of the lattice.
    #'
    #' @param indices (numeric or logical vector) The indices of the bonds (concepts) to extract. If \code{NULL} (default), extracts all.
    #'
    #' @return
    #' A list of \code{FormalContext} objects, each representing one bond.
    #'
    #' @export
    get_bonds = function(indices = NULL) {
      if (self$is_empty()) return(list())

      # Extents of BondLattice are formal concepts, intents are the bonds
      bonds_intents <- self$intents()
      
      G1 <- private$fc1$objects
      M2 <- private$fc2$attributes
      # Reconstruct attribute names to be safe and independent of parent private scope
      all_attr_names <- paste(rep(G1, times = length(M2)), rep(M2, each = length(G1)), sep = "_")

      bonds_list <- list()

      cols_to_extract <- seq_len(ncol(bonds_intents))
      if (!is.null(indices)) {
        if (inherits(indices, "ConceptSet")) {
          # If it's a ConceptSet, we iterate but we need to be careful.
          # The easiest way is to use its intents matrix
          bonds_intents <- indices$intents()
          cols_to_extract <- seq_len(ncol(bonds_intents))
        } else if (is.logical(indices)) {
          cols_to_extract <- which(indices)
        } else {
          cols_to_extract <- indices
        }
      }

      for (i in cols_to_extract) {
        v <- bonds_intents[, i]
        intent_names <- all_attr_names[which(v > 0)]

        # Determine dimensions and rebuild boolean matrix
        # the intent names are {g_1}_{m_1} as flattened
        mat <- matrix(0, nrow = length(G1), ncol = length(M2))
        rownames(mat) <- G1
        colnames(mat) <- M2

        for (name in intent_names) {
          parts <- strsplit(name, "_", fixed = TRUE)[[1]]
          g <- parts[1]
          m <- paste(parts[-1], collapse = "_") # in case attribute had an underscore
          mat[g, m] <- 1
        }

        # Return a custom FormalContext
        fc_bond <- FormalContext$new(mat)
        bonds_list <- append(bonds_list, list(fc_bond))
      }

      return(bonds_list)
    },

    #' @description
    #' Compute similarity, affinity, or complexity metrics between the two contexts.
    #'
    #' @param type (character) The type of metric to compute:
    #' \itemize{
    #'   \item \code{"log-bond"}: (Default) Normalized log-ratio of bonds. High value means high logical affinity.
    #'   \item \code{"top-density"}: Density of the largest possible bond (the top of the lattice).
    #'   \item \code{"complexity"}: Ratio of irreducible bonds to total bonds. Low value implies high structural emergence.
    #'   \item \code{"core-agreement"}: Ratio of filled cells in the Core bond versus the Top bond. Measures fundamental consensus.
    #'   \item \code{"entropy"}: Interaction entropy based on the log-size of the lattices.
    #'   \item \code{"stability"}: Average stability of the bonds in the lattice.
    #' }
    #'
    #' @return A numeric value representing the metric.
    #' @export
    similarity = function(type = c("log-bond", "top-density", "complexity", "core-agreement", "entropy", "stability", "width", "dimension", "width-index", "dimension-index")) {
      type <- match.arg(type)

      if (!is.null(private$cached_similarities[[type]])) {
        return(private$cached_similarities[[type]])
      }

      val <- switch(type,
        "log-bond" = {
          b12 <- self$size()
          b11 <- bonds(private$fc1, private$fc1)$size()
          b22 <- bonds(private$fc2, private$fc2)$size()
          if (b12 <= 1) { 0 }
          else if (b11 <= 1 || b22 <= 1) { 1 }
          else {
            denom <- sqrt(log(b11) * log(b22))
            if (denom == 0) 1 else log(b12) / denom
          }
        },
        "top-density" = {
          private$build_adjacency()
          bottom_idx <- which(Matrix::colSums(private$subconcept_matrix) == 1)
          bonds_intents <- self$intents()
          max_intent <- bonds_intents[, bottom_idx[1]]
          sum(max_intent > 0) / (length(private$fc1$objects) * length(private$fc2$attributes))
        },
        "complexity" = {
          length(self$join_irreducibles()) / self$size()
        },
        "core-agreement" = {
          private$build_adjacency()
          bottom_idx <- which(Matrix::colSums(private$subconcept_matrix) == 1)
          top_idx <- which(Matrix::colSums(private$subconcept_matrix) == self$size())
          bonds_intents <- self$intents()
          max_intent <- bonds_intents[, bottom_idx[1]]
          core_intent <- bonds_intents[, top_idx[1]]
          sum(core_intent > 0) / sum(max_intent > 0)
        },
        "entropy" = {
          b12 <- self$size()
          b11 <- bonds(private$fc1, private$fc1)$size()
          b22 <- bonds(private$fc2, private$fc2)$size()
          if (b12 <= 1) { 0 }
          else {
            denom <- log(b11) + log(b22)
            if (denom == 0) 1 else log(b12) / denom
          }
        },
        "stability" = {
          mean(super$stability())
        },
        "width" = {
          super$width()
        },
        "dimension" = {
          super$dimension()
        },
        "width-index" = {
          if (self$size() == 0) 0 else super$width() / self$size()
        },
        "dimension-index" = {
          sz <- self$size()
          if (sz <= 1) 1 else super$dimension() / log2(sz)
        }
      )

      private$cached_similarities[[type]] <- val
      return(val)
    },

    #' @description
    #' Get the 'Core' bond (the smallest possible bond).
    #'
    #' @return A \code{FormalContext} representing the minimal bond.
    #' @export
    get_core = function() {
      # The core bond is the intent of the top concept of the lattice
      private$build_adjacency()
      top_idx <- which(Matrix::colSums(private$subconcept_matrix) == self$size())
      return(self$get_bonds(top_idx[1])[[1]])
    },

    #' @description
    #' Print the BondLattice object.
    #'
    #' @return Nothing, just prints the object summary.
    #' @export
    print = function() {
      if (self$is_empty()) {
        cat("An empty Bond Lattice.\n")
        return(invisible(self))
      }
      cat("Bond Lattice between two formal contexts:\n")
      cat(sprintf("- Context 1 (G1): %d objects (%s...)\n", 
                  length(private$fc1$objects), 
                  paste(head(private$fc1$objects, 2), collapse=", ")))
      cat(sprintf("- Context 2 (M2): %d attributes (%s...)\n", 
                  length(private$fc2$attributes), 
                  paste(head(private$fc2$attributes, 2), collapse=", ")))
      cat("- Total Bonds:", self$size(), "\n")
      
      c_sim <- private$cached_similarities
      
      if (!is.null(c_sim[["log-bond"]])) {
        cat("- Logical Affinity (Log-Bond):", round(c_sim[["log-bond"]], 4), "\n")
      }
      if (!is.null(c_sim[["entropy"]])) {
        cat("- Interaction Entropy:", round(c_sim[["entropy"]], 4), "\n")
      }
      if (!is.null(c_sim[["complexity"]])) {
        cat("- Structural Complexity (JI/Bonds):", round(c_sim[["complexity"]], 4), "\n")
      }
      if (!is.null(c_sim[["core-agreement"]])) {
        cat("- Core Agreement Ratio:", round(c_sim[["core-agreement"]], 4), "\n")
      }
      if (!is.null(c_sim[["stability"]])) {
        cat("- Average Bond Stability:", round(c_sim[["stability"]], 4), "\n")
      }
      if (!is.null(c_sim[["width"]])) {
        cat("- Dilworth's Width:", c_sim[["width"]], 
            sprintf("(Index: %.4f)\n", c_sim[["width-index"]]))
      }
      if (!is.null(c_sim[["dimension"]])) {
        cat("- Order Dimension:", c_sim[["dimension"]], 
            sprintf("(Index: %.4f)\n", c_sim[["dimension-index"]]))
      }
      
      invisible(self)
    },

    #' @description
    #' Verify if a relation is a bond between the internal contexts.
    #'
    #' @param relation (matrix or FormalContext) The relation to verify.
    #'
    #' @return TRUE if it's a bond, FALSE otherwise.
    #' @export
    is_bond = function(relation) {
      # Use the standalone function
      return(is_bond(private$fc1, private$fc2, relation))
    }

  ),
  private = list(
    cached_similarities = list(),
    pr_extents = NULL,
    pr_intents = NULL,
    objects = NULL,
    attributes = NULL,
    I = NULL,
    concept_support = NULL,
    fc1 = NULL,
    fc2 = NULL
  )
)
