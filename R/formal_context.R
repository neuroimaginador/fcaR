#' @title
#' R6 class for a formal context
#'
#' @description
#' This class implements the data structure and methods for formal contexts.
#'
#' @section Public fields:
#' * `I`: the table for the formal context.
#' * `attributes`: name of the attributes in the formal context.
#' * `objects`: name of the objects in the context.
#' * `grades_set`: set of grades (in [0, 1]) of the attributes.
#' * `concepts`: list of concepts (extent, intent).
#' * `implications`: extracted implications as an \code{ImplicationSet}.
#' * `concept_support`: vector with the support of the concepts.
#' * `implications_support`: vector with support of the extracted implications.
#'
#' @importFrom methods as is slotNames
#' @export
formal_context <- R6::R6Class(

  classname = "FormalContext",

  public = list(

    I = NULL,

    attributes = NULL,

    objects = NULL,

    grades_set = NULL,

    expanded_grades_set = NULL,

    concepts = NULL,

    implications = NULL,

    concept_support = NULL,

    implications_support = NULL,

    #' @description
    #' Creator for the Fomal Context class
    #'
    #' @param I           (numeric matrix) The table of the formal context.
    #' @param grades_set  (numeric vector, optional) the anumeration of the grades of the attributes.
    #'
    #' @details
    #' Columns of \code{I} must be named, and are the names of the attributes of the formal context.
    #'
    #' @return An object of the \code{FormalContext} class.
    #' @export
    #'
    #' @import Matrix
    #' @import arules
    #' @importFrom methods as is slotNames
    initialize = function(I,
                          grades_set = sort(unique(as.vector(I)))) {

      # Transform the formal context to sparse
      if (inherits(I, "transactions")) {

        # If it comes from the arules package
        attributes <- I@itemInfo$labels
        I <- as(I@data, "dgCMatrix")
        objects <- paste0(seq(ncol(I)))

      } else {

        # Or if it comes from a numeric table
        attributes <- colnames(I)
        objects <- rownames(I)

        # Remove the constant columns
        constant_cols <- which(apply(I, 2, max) == apply(I, 2, min))

        if (length(constant_cols) > 0) {

          message(paste0("Removed constant columns: ", str_flatten(attributes[constant_cols], collapse = ", ")))

          I <- I[, -constant_cols]
          attributes <- attributes[-constant_cols]

        }

        I <- as(Matrix(t(I),
                       sparse = TRUE), "dgCMatrix")

      }

      # Assign everything to its corresponding field
      expanded_grades_set <- compute_grades(t(I))

      self$I <- I
      self$grades_set <- grades_set
      self$expanded_grades_set <- expanded_grades_set
      self$objects <- colnames(I)
      self$attributes <- attributes

    },

    #' @description
    #' Add a precomputed implication set
    #'
    #' @param impl_set   (\code{ImplicationSet} object) The implications to add to this formal context.
    #'
    #' @return Nothing, just updates the internal \code{implications} field.
    #'
    #' @import arules
    #' @export
    add_implications = function(impl_set) {

      if (inherits(impl_set, "rules")) {

        # If it comes from arules
        # convert to our format
        implications <- implication_set$new()
        implications$from_arules(impl_set)

      } else {

        # If it's already an implication set
        # just clone it
        implications <- impl_set$clone()

      }

      # Initialize or add the implications field
      if (is.null(self$implications)) {

        self$implications <- implications

      } else {

        old_LHS <- self$implications$get_LHS_matrix()
        old_RHS <- self$implications$get_RHS_matrix()

        new_LHS <- implications$get_LHS_matrix()
        new_RHS <- implications$get_LHS_matrix()

        LHS <- cbind(old_LHS, new_LHS)
        RHS <- cbind(old_RHS, new_RHS)

        impl <- implication_set$new(attributes = self$attributes,
                                    lhs = LHS,
                                    rhs = RHS)

        self$implications <- impl

      }

    },

    #' @description
    #' Use Ganter Algorithm to compute concepts
    #'
    #' @param verbose   (logical) TRUE will provide a verbose output.
    #'
    #' @return A list with all the concepts in the formal context.
    #' @export
    compute_concepts = function(verbose = FALSE) {

      # If already computed, no need to compute them again
      if (!is.null(self$concepts)) return(self$concepts)

      self$concepts <- .concepts(as.matrix(t(self$I)),
                                 self$grades_set,
                                 verbose = verbose,
                                 attributes = self$attributes)

      return(self$concepts)

    },

    #' @description
    #' Use modified Ganter algorithm to compute both concepts and implications
    #'
    #' @param verbose   (logical) TRUE will provide a verbose output.
    #'
    #' @return Nothing, just updates the internal fields \code{concepts} and \code{implications}.
    #'
    #' @export
    extract_implications_concepts = function(verbose = FALSE) {

      my_I <- as.matrix(t(self$I))
      grades_set <- rep(list(self$grades_set), length(self$attributes))
      attrs <- self$attributes

      L <- next_closure_implications(I = my_I,
                                     grades_set = grades_set,
                                     attrs = attrs,
                                     verbose = verbose)

      # Since the previous function gives the list of intents of
      # the computed concepts, now we will compute the corresponding
      # extents.
      my_intents <- L$concepts[, -1]

      my_concepts <- list()

      for (n in seq(ncol(my_intents))) {

        intent <- .extract_column(my_intents, n)
        extent <- .extent(intent, my_I)

        my_concepts <- c(my_concepts, list(list(extent, intent)))

      }

      # Now, add the computed implications
      my_LHS <- L$LHS[, -1]
      my_RHS <- L$RHS[, -1]

      self$concepts <- my_concepts

      extracted_implications <- implication_set$new(attributes = self$attributes,
                                                    lhs = my_LHS,
                                                    rhs = my_RHS)

      self$implications <- extracted_implications

    },

    #' @description
    #' Convert the formal context to object of class \code{transactions} from the \code{arules} package
    #'
    #' @return A \code{transactions} object.
    #'
    #' @importFrom methods as
    #' @import arules
    #' @export
    convert_to_transactions = function() {

      return(as(as(self$I, "ngCMatrix"), "transactions"))

    },

    #' @description
    #' Export implications to \code{arules} format
    #'
    #' @param quality   (logical) Compute the interest measures for each rule?
    #'
    #' @return A \code{rules} object from \code{arules} package. If \code{quality == TRUE}, the function \code{interestMeasure} from \code{arules} is used.
    #'
    #' @import arules
    #' @importFrom methods as
    #'
    #' @export
    export_implications_to_arules = function(quality = TRUE) {

      R <- self$implications$to_arules()

      if (quality) {

        quality(R) <- interestMeasure(R,
                                      transactions = as(as(self$I, "ngCMatrix"), "transactions"))

      }

      return(R)

    },

    #' @description
    #' Plot the concept lattice
    #'
    #' @return Nothing, just plots the graph of the concept lattice.
    #' @export
    plot_lattice = function() {

      if (length(self$concepts) > 0) {

        .draw_lattice(self$concepts, as.matrix(self$I))

      }

    },

    #' @description
    #' Plot the formal context table
    #'
    #' @return Nothing, just plots the formal context.
    #'
    #' @import scales RColorBrewer
    #'
    #' @export
    plot_context = function() {

      color_function <- colour_ramp(brewer.pal(9, "Greys"))
      heatmap(t(as.matrix(self$I)), Rowv = NA, Colv = NA,
              col = color_function(seq(0, 1, 0.01)),
              scale = "none")

    },

    #' @description
    #' Get support of each concept
    #'
    #' @return A vector with the support of each concept.
    #' @export
    get_concept_support = function() {

      my_I <- self$I
      my_I@x <- as.numeric(my_I@x)

      intents <- lapply(self$concepts, function(s) s[[2]])
      intents <- do.call(cbind, args = intents)

      subsets <- .subset(intents, my_I)

      self$concept_support <- rowMeans(subsets)

      return(self$concept_support)

    },

    #' @description
    #' Compute support of each implication
    #'
    #' @return A vector with the support of each implication
    #' @export
    get_implication_support = function() {

      LHS <- self$implications$get_LHS_matrix()
      my_I <- self$I

      subsets <- .subset(LHS, my_I)

      self$implications_support <- rowMeans(subsets)

      return(self$implications_support)

    }

  )

)
