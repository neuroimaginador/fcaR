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
#' * `grades_set`: set of grades (in \[0, 1\]) of the attributes.
#' * `concepts`: list of concepts (extent, intent).
#' * `implications`: extracted implications as an \code{ImplicationSet}.
#' * `concept_support`: vector with the support of the concepts.
#' * `implications_support`: vector with support of the extracted implications.
#'
#' @references
#'
#' Guigues J, Duquenne V (1986). “Familles minimales d'implications informatives résultant d'un tableau de données binaires.” _Mathématiques et Sciences humaines_, *95*, 5-18.
#'
#' Ganter B, Wille R (1999). _Formal concept analysis : mathematical foundations_. Springer. ISBN 3540627715.
#'
#' Belohlavek R (2002). “Algorithms for fuzzy concept lattices.” In _Proc. Fourth Int. Conf. on Recent Advances in Soft Computing_. Nottingham, United Kingdom, 200-205.
#'
#' Hahsler M, Grun B, Hornik K (2005). “arules - a computational environment for mining association rules and frequent item sets.” _J Stat Softw_, *14*, 1-25.
#'
#' @importFrom methods as is slotNames
#' @export
FormalContext <- R6::R6Class(

  classname = "FormalContext",

  public = list(

    I = NULL,

    attributes = NULL,

    objects = NULL,

    grades_set = NULL,

    expanded_grades_set = NULL,

    concepts = NULL,

    implications = NULL,

    implications_support = NULL,

    #' @description
    #' Creator for the Fomal Context class
    #'
    #' @param I           (numeric matrix) The table of the formal context.
    #' @param remove_const (logical) If \code{TRUE}, remove constant columns. The default is \code{FALSE}.
    #'
    #' @details
    #' Columns of \code{I} should be named, since they are the names of the attributes of the formal context.
    #'
    #' If no \code{I} is used, the resulting \code{FormalContext} will be empty and not usable unless for loading a previously saved one.
    #'
    #' @return An object of the \code{FormalContext} class.
    #' @export
    #'
    #' @import Matrix
    #' @import arules
    #' @importFrom stringr str_wrap
    #' @importFrom methods as is slotNames
    initialize = function(I,
                          remove_const = FALSE) {

      if (missing(I)) {

        return(invisible(self))

      }

      # Transform the formal context to sparse
      if (inherits(I, "transactions")) {

        # If it comes from the arules package
        attributes <- I@itemInfo$labels
        I <- as(I@data, "dgCMatrix")
        objects <- paste0(seq(ncol(I)))
        dimnames(I) <- list(attributes, objects)

      } else {

        # Or if it comes from a numeric table
        if (length(colnames(I)) > 0) {

          attributes <- colnames(I)

        } else {

          attributes <- paste0("A", seq(ncol(I)))

        }

        if (length(rownames(I)) > 0) {

          objects <- rownames(I)

        } else {

          objects <- paste0("A", seq(nrow(I)))

        }

        # Remove the constant columns
        if (remove_const) {

          constant_cols <- which(apply(I, 2, max) == apply(I, 2, min))

          if (length(constant_cols) > 0) {

            str <- paste0("Removed constant columns: ", str_flatten(attributes[constant_cols], collapse = ", "))

            message(str_wrap(str,
                             exdent = 2,
                             width = 75))

            I <- I[, -constant_cols]
            attributes <- attributes[-constant_cols]

          }

        }

        I <- as(Matrix(t(I),
                       sparse = TRUE), "dgCMatrix")

      }

      # Assign everything to its corresponding field
      expanded_grades_set <- compute_grades(t(I))
      grades_set <- sort(unique(unlist(expanded_grades_set)))

      self$I <- I
      self$grades_set <- unique(c(0, grades_set, 1))
      self$expanded_grades_set <- expanded_grades_set
      self$objects <- objects
      self$attributes <- attributes

      # Is the FormalContext binary?
      private$is_binary <- length(self$grades_set) == 2

      # Create a new empty implication set inside
      self$implications <- ImplicationSet$new(attributes = attributes)

      # Create a new empty ConceptLattice inside
      self$concepts <- ConceptLattice$new(extents = NULL,
                                          intents = NULL,
                                          objects = self$objects,
                                          attributes = self$attributes,
                                          I = self$I)

    },

    #' @description
    #' Check if the \code{FormalContext} is empty
    #'
    #' @return \code{TRUE} if the \code{FormalContext} is empty, that is, has not been provided with a matrix, and \code{FALSE} otherwise.
    #'
    #' @export
    is_empty = function() {

      return(is.null(self$I))

    },

    #' @description
    #' Get the intent of a fuzzy set of objects
    #'
    #' @param S   (\code{SparseSet}) The set of objects to compute the intent for.
    #'
    #' @return A \code{SparseSet} with the intent.
    #'
    #' @importFrom Matrix sparseMatrix
    #'
    #' @export
    get_intent = function(S) {

      if (inherits(S, "SparseSet")) {

        if (all(S$get_attributes() == self$objects)) {

          S <- S$get_vector()

        } else {

          stop("It is not a set of the required type (set of objects).", call. = FALSE)

        }

      }

      if (length(S) == length(self$objects)) {

        R <- compute_intent(S, as.matrix(t(self$I)))

        if (length(R@i) > 0) {

          # Non-empty set:
          R <- sparseMatrix(i = R@i + 1,
                            j = rep(1, length(R@i)),
                            x = R@x,
                            dims = c(length(self$attributes), 1))

          R <- SparseSet$new(attributes = self$attributes,
                             M = R)
        } else {

          # Empty intent
          R <- SparseSet$new(attributes = self$attributes)

        }

        return(R)

      } else {

        stop("It is not a set of the required type (set of objects).", call. = FALSE)

      }

    },

    #' @description
    #' Get the extent of a fuzzy set of attributes
    #'
    #' @param S   (\code{SparseSet}) The set of attributes to compute the extent for.
    #'
    #' @return A \code{SparseSet} with the intent.
    #'
    #' @importFrom Matrix sparseMatrix
    #'
    #' @export
    get_extent = function(S) {

      if (inherits(S, "SparseSet")) {

        if (all(S$get_attributes() == self$attributes)) {

          S <- S$get_vector()

        } else {

          stop("It is not a set of the required type (set of attributes).", call. = FALSE)

        }

      }

      if (length(S) == length(self$attributes)) {

        R <- compute_extent(S, as.matrix(t(self$I)))

        if (length(R@i) > 0) {

          # Non-empty set:
          R <- sparseMatrix(i = R@i + 1,
                            j = rep(1, length(R@i)),
                            x = R@x,
                            dims = c(length(self$objects), 1))

          R <- SparseSet$new(attributes = self$objects,
                             M = R)
        } else {

          # Empty extent
          R <- SparseSet$new(attributes = self$objects)

        }

        return(R)

      } else {

        stop("It is not a set of the required type (set of objects).", call. = FALSE)

      }

    },

    #' @description
    #' Get the closure of a fuzzy set of attributes
    #'
    #' @param S   (\code{SparseSet}) The set of attributes to compute the closire for.
    #'
    #' @return A \code{SparseSet} with the closure.
    #'
    #' @importFrom Matrix sparseMatrix
    #'
    #' @export
    get_closure = function(S) {

      if (inherits(S, "SparseSet")) {

        if (all(S$get_attributes() == self$attributes)) {

          S <- S$get_vector()

        } else {

          stop("It is not a set of the required type (set of attributes).", call. = FALSE)

        }

      }

      if (length(S) == length(self$attributes)) {

        R <- compute_closure(S, as.matrix(t(self$I)))

        if (length(R@i) > 0) {

          # Non-empty set:
          R <- sparseMatrix(i = R@i + 1,
                            j = rep(1, length(R@i)),
                            x = R@x,
                            dims = c(length(self$attributes), 1))

          R <- SparseSet$new(attributes = self$attributes,
                             M = R)
        } else {

          # Empty closure
          R <- SparseSet$new(attributes = self$attributes)

        }

        return(R)

      } else {

        stop("It is not a set of the required type (set of objects).", call. = FALSE)

      }

    },

    #' @description
    #' Clarify a formal context
    #'
    #' @param copy   (logical) If \code{TRUE}, a new \code{FormalContext} object is created with the clarified context, otherwise the current one is overwritten.
    #'
    #' @return The clarified \code{FormalContext}.
    #'
    #' @export
    clarify = function(copy = FALSE) {

      # Redundant attributes
      my_I <- .clarify_matrix(t(self$I),
                              rows = self$objects,
                              cols = self$attributes)

      # And redundant objects
      my_I <- .clarify_matrix(t(my_I),
                              rows = colnames(my_I),
                              cols = self$objects)
      my_I <- as.matrix(t(my_I))

      if (copy) {

        fc2 <- FormalContext$new(my_I)

        return(fc2)

      } else {

        self$initialize(my_I)

        return(invisible(self))

      }

    },

    #' @description
    #' Reduce a formal context
    #'
    #' @param copy   (logical) If \code{TRUE}, a new \code{FormalContext} object is created with the clarified and reduced context, otherwise the current one is overwritten.
    #'
    #' @return The clarified and reduced \code{FormalContext}.
    #'
    #' @export
    reduce = function(copy = FALSE) {

      if (!private$is_binary) {

        stop("This FormalContext is not binary. Reduction is not implemented for fuzzy contexts.", call. = FALSE)

      }

      # Make a copy with the clarified context
      fc2 <- self$clarify(TRUE)

      my_I <- as.matrix(t(fc2$I))

      att <- fc2$attributes

      Z <- SparseSet$new(attributes = att)

      for (y in att) {

        R <- SparseSet$new(attributes = fc2$objects)
        R$assign(attributes = fc2$objects,
                 values = rep(1, length(fc2$objects)))

        R <- R$get_vector()

        yv <- SparseSet$new(attributes = att)
        yv$assign(attributes = y, values = 1)
        y_down <- fc2$get_extent(yv)

        for (yp in setdiff(att, y)) {

          ypv <- SparseSet$new(attributes = att)
          ypv$assign(attributes = yp, values = 1)
          yp_down <- fc2$get_extent(ypv)

          S <- .subset(y_down$get_vector(),
                       yp_down$get_vector())

          if (S[1]) {

            R[Matrix::which(yp_down$get_vector() < R)] <- yp_down$get_vector()[Matrix::which(yp_down$get_vector() < R)]

          }

        }

        if (!.equal_sets(R, y_down$get_vector())[1]) {

          Z$assign(attributes = y, values = 1)

        }

      }

      new_att <- Z$get_attributes()[Matrix::which(Z$get_vector() > 0)]

      idx <- match(new_att, att)
      my_I <- my_I[, idx]
      colnames(my_I) <- new_att
      rownames(my_I) <- fc2$objects

      if (copy) {

        fc3 <- FormalContext$new(my_I)

        return(fc3)

      } else {

        self$initialize(my_I)

        return(invisible(self))

      }

    },

    standardize = function() {

      if (self$concepts$is_empty()) {

        stop("Concepts must be computed beforehand.\n", call. = FALSE)

      }

      join_irr <- self$concepts$join_irreducibles()
      meet_irr <- self$concepts$meet_irreducibles()

      nj <- length(join_irr)
      nm <- length(meet_irr)

      I <- matrix(0, nrow = nj, ncol = nm)

      for (i in seq(nj)) {

        for (j in seq(nm)) {

          I[i, j] <- ifelse(join_irr[[i]] %<=% meet_irr[[j]], 1, 0)

        }

      }

      colnames(I) <- paste0("M", seq(nm))
      rownames(I) <- paste0("J", seq(nj))

      return(FormalContext$new(I))

    },

    #' @description
    #' Use Ganter Algorithm to compute concepts
    #'
    #' @param verbose   (logical) TRUE will provide a verbose output.
    #'
    #' @return A list with all the concepts in the formal context.
    #'
    #' @export
    compute_concepts = function(verbose = FALSE) {

      private$check_empty()

      my_I <- as.matrix(t(self$I))
      grades_set <- rep(list(self$grades_set), length(self$attributes))
      # grades_set <- self$expanded_grades_set
      attrs <- self$attributes

      L <- next_closure_concepts(I = my_I,
                                     grades_set = grades_set,
                                     attrs = attrs,
                                     verbose = verbose)

      # Since the previous function gives the list of intents of
      # the computed concepts, now we will compute the corresponding
      # extents.
      my_intents <- L$concepts[, -1]
      my_extents <- L$extents[, -1]

      self$concepts <- ConceptLattice$new(extents = my_extents,
                                          intents = my_intents,
                                          objects = self$objects,
                                          attributes = self$attributes,
                                          I = self$I)

      return(invisible(self$concepts))

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

      private$check_empty()

      if (inherits(impl_set, "rules")) {

        # If it comes from arules
        # convert to our format
        implications <- ImplicationSet$new(impl_set)

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

        impl <- ImplicationSet$new(attributes = self$attributes,
                                   lhs = LHS,
                                   rhs = RHS)

        self$implications <- impl

      }

    },

    #' @description
    #' Use modified Ganter algorithm to compute both concepts and implications
    #'
    #' @param verbose   (logical) TRUE will provide a verbose output.
    #'
    #' @return Nothing, just updates the internal fields \code{concepts} and \code{implications}.
    #'
    #'
    #' @export
    extract_implications_concepts = function(verbose = FALSE) {

      private$check_empty()

      my_I <- as.matrix(t(self$I))
      grades_set <- rep(list(self$grades_set), length(self$attributes))
      # grades_set <- self$expanded_grades_set
      attrs <- self$attributes

      L <- next_closure_implications(I = my_I,
                                     grades_set = grades_set,
                                     attrs = attrs,
                                     verbose = verbose)

      # Since the previous function gives the list of intents of
      # the computed concepts, now we will compute the corresponding
      # extents.
      my_intents <- L$concepts[, -1]
      my_extents <- L$extents[, -1]

      # Now, add the computed implications
      my_LHS <- L$LHS[, -1]
      my_RHS <- L$RHS[, -1]

      self$concepts <- ConceptLattice$new(extents = my_extents,
                                          intents = my_intents,
                                          objects = self$objects,
                                          attributes = self$attributes,
                                          I = self$I)

      extracted_implications <- ImplicationSet$new(attributes = self$attributes,
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
    #'
    #' @export
    convert_to_transactions = function() {

      private$check_empty()

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

      private$check_empty()

      R <- self$implications$to_arules()

      if (quality) {

        quality(R) <- interestMeasure(R,
                                      transactions = as(as(self$I, "ngCMatrix"), "transactions"))

      }

      return(R)

    },

    #' @description
    #' Save a \code{FormalContext} to RDS format
    #'
    #' @param filename   (character) Path of the RDS file where to store the \code{FormalContext}.
    #'
    #' @return Invisibly the current \code{FormalContext}.
    #'
    #' @export
    save = function(filename = tempfile(fileext = ".rds")) {

      private$check_empty()

      if (!self$concepts$is_empty()) {

        extents <- self$concepts$get_extents()
        intents <- self$concepts$get_intents()

      } else {

        extents <- NULL
        intents <- NULL

      }

      L <- list(I = self$I,
                extents = extents,
                intents = intents,
                attributes = self$attributes,
                objects = self$objects,
                expanded_grades_set = self$expanded_grades_set,
                grades_set = self$grades_set,
                implications = self$implications,
                implications_support = self$implications_support)

      saveRDS(L, file = filename)

      return(invisible(self))

    },

    #' @description
    #' Load a \code{FormalContext} from a RDS file
    #'
    #' @param filename   (character) Path of the RDS file to load the \code{FormalContext} from.
    #'
    #' @return The loaded \code{FormalContext}.
    #'
    #' @export
    load = function(filename) {

      L <- readRDS(filename)

      self$I <- L$I
      self$attributes <- L$attributes
      self$objects <- L$objects
      self$expanded_grades_set <- L$expanded_grades_set
      self$grades_set <- L$grades_set
      self$implications <- L$implications
      self$implications_support <- L$implications_support

      if (!is.null(L$extents)) {

        self$concepts <- ConceptLattice$new(extents = L$extents,
                                            intents = L$intents,
                                            objects = L$objects,
                                            attributes = L$attributes,
                                            I = L$I)

      }

      return(invisible(self))

    },

    #' @description
    #' Dimensions of the formal context
    #'
    #' @return A vector with (number of objects, number of attributes).
    #'
    #' @export
    dim = function() {

      return(c(length(self$objects), length(self$attributes)))

    },

    #' @description
    #' Prints the formal context
    #'
    #' @return Prints information regarding the formal context.
    #' @importFrom stringr str_flatten str_wrap
    #' @export
    print = function() {

      if (self$is_empty()) {

        cat("Empty FormalContext.\n")

        return(invisible(self))

      }

      dims <- self$dim()

      I <- as.matrix(t(self$I))

      if (dims[2] > 6) {

        my_attributes <- c(self$attributes[1:6], "...")
        warning("Too many attributes, output will be truncated.\n",
                call. = FALSE,
                noBreaks. = FALSE,
                immediate. = TRUE)

      } else {

        my_attributes <- self$attributes

      }

      cat("FormalContext with", dims[1], "objects and",
          dims[2], "attributes.\n")

      str <- paste0("Attributes' names are: ",
                    str_flatten(my_attributes, collapse = ", "),
                    "\n")
      cat(str_wrap(str, exdent = 2))
      cat("\nMatrix:\n")

      print(head(I[, seq_along(my_attributes)]))

    },

    #' @description
    #' Plot the formal context table
    #'
    #' @return Nothing, just plots the formal context.
    #'
    #' @import scales RColorBrewer
    #'
    #' @export
    plot = function() {

      private$check_empty()

      color_function <- colour_ramp(brewer.pal(9, "Greys"))
      heatmap(t(as.matrix(self$I)), Rowv = NA, Colv = NA,
              col = color_function(seq(0, 1, 0.01)),
              scale = "none")

    },

    #' @description
    #' Compute support of each implication
    #'
    #' @return A vector with the support of each implication
    #' @export
    get_implication_support = function() {

      private$check_empty()

      if (is.null(self$implications) ||
          (self$implications$cardinality() == 0)) {

        return(invisible(NULL))

      }

      LHS <- self$implications$get_LHS_matrix()
      my_I <- self$I

      subsets <- .subset(LHS, my_I)

      self$implications_support <- rowMeans(subsets)

      return(self$implications_support)

    }

  ),

  private = list(

    is_binary = FALSE,

    check_empty = function() {

      if (self$is_empty()) {

        stop("The formal context is empty. The only allowed method is 'load' to import from a previously saved RDS file.", call. = FALSE)

      }

      return(invisible(NULL))

    }

  )



)
