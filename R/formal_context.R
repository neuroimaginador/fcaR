#' @title
#' R6 class for a formal context
#'
#' @description
#' This class implements the data structure and methods for formal contexts.
#'
#' @examples
#' # Build and print the formal context
#' fc_planets <- FormalContext$new(planets)
#' print(fc_planets)
#'
#' # Define a set of attributes
#' S <- Set$new(attributes = fc_planets$attributes)
#' S$assign(moon = 1, large = 1)
#'
#' # Compute the closure of S
#' Sc <- fc_planets$closure(S)
#' # Is Sc a closed set?
#' fc_planets$is_closed(Sc)
#'
#' # Clarify and reduce the formal context
#' fc2 <- fc_planets$reduce(TRUE)
#'
#' # Find implications
#' fc_planets$find_implications()
#'
#' # Read a formal context from CSV
#' filename <- system.file("contexts", "airlines.csv", package = "fcaR")
#' fc <- FormalContext$new(filename)
#'
#' # Read a formal context from a CXT file
#' filename <- system.file("contexts", "lives_in_water.cxt", package = "fcaR")
#' fc <- FormalContext$new(filename)
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
#' @export
FormalContext <- R6::R6Class(
  classname = "FormalContext",
  public = list(
    #' @field I The table of the formal context as a matrix.
    I = NULL,

    #' @field attributes The attributes of the formal context.
    attributes = NULL,

    #' @field objects The objects of the formal context.
    objects = NULL,

    #' @field grades_set The set of degrees (in \[0, 1\]) the whole set of attributes can take.
    grades_set = NULL,

    #' @field expanded_grades_set The set of degrees (in \[0, 1\]) each attribute can take.
    expanded_grades_set = NULL,

    #' @field concepts The concept lattice associated to the formal context as a \code{\link{ConceptLattice}}.
    concepts = "Not computed yet",

    #' @field implications A set of implications on the formal context as an \code{\link{ImplicationSet}}.
    implications = "Not computed yet",

    #' @field description An optional description of the dataset
    description = character(0),

    #' @description
    #' Creator for the Formal Context class
    #'
    #' @param I           (numeric matrix) The table of the formal context.
    #' @param filename    (character) Path of a file to import.
    #' @param remove_const (logical) If \code{TRUE}, remove constant columns. The default is \code{FALSE}.
    #'
    #' @details
    #' Columns of \code{I} should be named, since they are the names of the attributes of the formal context.
    #'
    #' If no \code{I} is used, the resulting \code{FormalContext} will be empty and not usable unless for loading a previously saved one. In this case, one can provide a \code{filename} to import. Only RDS, CSV and CXT files are currently supported.
    #'
    #' If the file is not present, the fcarepository.org is looked for coincidences. If so, the corresponding context is loaded.
    #'
    #' @return An object of the \code{FormalContext} class.
    #' @export
    #'
    initialize = function(I, filename, remove_const = FALSE) {
      if (missing(I)) {
        if (!missing(filename) && file.exists(filename)) {
          self$load(filename)
        }

        return(invisible(self))
      }

      if ((length(I) == 1) && is.character(I) && file.exists(I)) {
        self$load(I)

        return(invisible(self))
      }

      if ((length(I) == 1) && is.character(I) && !file.exists(I)) {
        stop(
          glue::glue(
            "File '{I}' not found. If you want to download it from the repository, please use `fcaR::fetch_context('{I}')` instead."
          ),
          call. = FALSE
        )
      }

      # version

      if (!capabilities()["long.double"] & getRversion() < "4.1.0") {
        private$can_plot <- FALSE
      }

      # Transform the formal context to sparse
      if (inherits(I, "transactions")) {
        check_needed_pkg("arules", "process 'transactions' objects")

        # If it comes from the arules package
        attributes <- I@itemInfo$labels
        I <- convert_to_sparse(I@data)
        objects <- paste0(seq_len(ncol(I)))
        dimnames(I) <- list(attributes, objects)
      } else {
        # Or if it comes from a numeric table
        if (length(colnames(I)) > 0) {
          attributes <- colnames(I)
        } else {
          attributes <- paste0("A", seq_len(ncol(I)))
        }

        if (length(rownames(I)) > 0) {
          objects <- rownames(I)
        } else {
          objects <- paste0("O", seq_len(nrow(I)))
        }

        private$is_many_valued <- check_many_valued(I)

        if (!private$is_many_valued) {
          I <- as.matrix(I)

          # Remove the constant columns
          if (remove_const) {
            constant_cols <- which(apply(I, 2, max) == apply(I, 2, min))

            if (length(constant_cols) > 0) {
              str <- paste0(
                "Removed constant columns: ",
                stringr::str_flatten(attributes[constant_cols], collapse = ", ")
              )

              message(stringr::str_wrap(str, exdent = 2, width = 75))

              I <- I[, -constant_cols]
              attributes <- attributes[-constant_cols]
            }
          }

          # # TODO: save tI???
          # tI <- new_spm(I)
          # I <- new_spm(t(I))
          I <- convert_to_sparse(
            Matrix::Matrix(t(I), sparse = TRUE)
          )
        }
      }

      self$objects <- objects
      self$attributes <- attributes

      if (!private$is_many_valued) {
        # Assign everything to its corresponding field
        expanded_grades_set <- compute_grades(Matrix::t(I))
        grades_set <- sort(unique(unlist(expanded_grades_set)))

        self$I <- I
        self$grades_set <- unique(c(0, grades_set, 1))
        self$expanded_grades_set <- expanded_grades_set

        colnames(self$I) <- self$objects
        rownames(self$I) <- self$attributes

        # Is the FormalContext binary?
        private$is_binary <- length(self$grades_set) == 2
      } else {
        # browser()

        private$many_valued_I <- I
        # colnames(private$many_valued_I) <- self$objects
        # rownames(private$many_valued_I) <- self$attributes
      }

      # Create a new empty implication set inside
      self$implications <- ImplicationSet$new(
        attributes = attributes,
        I = self$I
      )

      # Create a new empty ConceptLattice inside
      self$concepts <- ConceptLattice$new(
        extents = NULL,
        intents = NULL,
        objects = self$objects,
        attributes = self$attributes,
        I = self$I
      )
    },

    #' @description
    #' Check if the \code{FormalContext} is empty
    #'
    #' @return \code{TRUE} if the \code{FormalContext} is empty, that is, has not been provided with a matrix, and \code{FALSE} otherwise.
    #'
    #' @export
    is_empty = function() {
      return(is.null(self$I) && is.null(private$many_valued_I))
    },

    #' @description
    #' Scale the context
    #'
    #' @param attributes  The attributes to scale
    #' @param type        Type of scaling.
    #'
    #' @param ...
    #'
    #' @details
    #' The types of scaling are implemented in a registry,
    #' so that \code{scalingRegistry$get_entries()} returns
    #' all types.
    #'
    #' In the dots argument, the user can supply the value for \code{bg} (logical), which, if set to \code{TRUE}, indicates to compute background knowledge as implications on the scales; if \code{FALSE}, no implications will be computed on the scales.
    #'
    #' @return The scaled formal context
    #' @export
    #' @examples
    #' filename <- system.file("contexts", "aromatic.csv", package = "fcaR")
    #' fc <- FormalContext$new(filename)
    #' fc$scale("nitro", "ordinal", comparison = `>=`, values = 1:3)
    #' fc$scale("OS", "nominal", c("O", "S"))
    #' fc$scale(attributes = "ring", type = "nominal")
    scale = function(attributes, type, ...) {
      # TODO: Check that the attributes are in self$attributes

      if (missing(attributes)) {
        attributes <- self$attributes
      }
      if (is.function(attributes)) {
        I <- self$incidence()

        attributes <- sapply(self$attributes, \(n) attributes(I[, n])) |>
          which() |>
          names()
      }

      bg <- FALSE
      dots <- list(...)
      if ("bg" %in% names(dots)) {
        bg <- dots$bg
        dots$bg <- NULL
      }

      I <- self$incidence()
      for (att in attributes) {
        scaled <-
          scale_context(I, column = att, type = type, bg = bg, dots)

        I <- scaled$derived
        private$scales <- c(
          private$scales,
          scaled$scale
        )
        names(private$scales)[length(private$scales)] <- att

        if (bg) {
          # Add implications to the bg_implications
          private$bg_implications <- combine_implications(
            private$bg_implications,
            scaled$bg_implications
          )

          # if (scaled$bg_implications$cardinality() > 0) {
          #
          #   private$bg_implications$to_basis()
          #
          # }
        }
      }

      self$initialize(I)
    },

    #' @description
    #' Scales applied to the formal context
    #'
    #' @param attributes (character) Name of the attributes for which scales
    #' (if applied) are returned.
    #'
    #' @return The scales that have been applied to the specified attributes
    #' of the formal context. If no \code{attributes} are passed,
    #' then all applied scales are returned.
    #'
    #' @export
    #' @examples
    #' filename <- system.file("contexts", "aromatic.csv", package = "fcaR")
    #' fc <- FormalContext$new(filename)
    #' fc$scale("nitro", "ordinal", comparison = `>=`, values = 1:3)
    #' fc$scale("OS", "nominal", c("O", "S"))
    #' fc$scale(attributes = "ring", type = "nominal")
    #' fc$get_scales()
    get_scales = function(attributes = names(private$scales)) {
      # TODO: Get the scales applied to only one or some
      # of the self$attributes.
      attributes <- attributes[attributes %in% names(private$scales)]

      res <- private$scales[attributes]
      if (length(res) > 0) {
        if (length(res) > 1) {
          return(res)
        } else {
          return(res[[1]])
        }
      } else {
        message("No scaling has been performed on these attributes.")
      }
    },

    #' @description
    #' Background knowledge of a scaled formal context
    #'
    #' @return
    #' An \code{ImplicationSet} with the implications
    #' extracted from the application of scales.
    #'
    #' @export
    #'
    #' @examples
    #' filename <- system.file("contexts", "aromatic.csv", package = "fcaR")
    #' fc <- FormalContext$new(filename)
    #' fc$scale("nitro", "ordinal", comparison = `>=`, values = 1:3)
    #' fc$scale("OS", "nominal", c("O", "S"))
    #' fc$scale(attributes = "ring", type = "nominal")
    #' fc$background_knowledge()
    background_knowledge = function() {
      if (!is.null(private$bg_implications)) {
        if (is.null(private$bg_implications_basis)) {
          private$bg_implications_basis <- private$bg_implications$clone()
          # private$bg_implications_basis$to_basis()
          suppressMessages(private$bg_implications_basis$apply_rules(c(
            "comp",
            "simp",
            "rsimp"
          )))
        }

        private$bg_implications_basis$clone()

        # private$bg_implications$clone()
      } else {
        ImplicationSet$new(attributes = self$attributes)
      }
    },

    #' @description
    #' Get the dual formal context
    #'
    #' @return A \code{FormalContext} where objects and attributes have interchanged their roles.
    #'
    #' @export
    dual = function() {
      if (private$is_many_valued) {
        return(FormalContext$new(t(private$many_valued_I)))
      } else {
        I <- Matrix::as.matrix(self$I)
        colnames(I) <- self$objects
        rownames(I) <- self$attributes

        return(FormalContext$new(I))
      }
    },

    #' @description
    #' Get the intent of a fuzzy set of objects
    #'
    #' @param S   (\code{Set}) The set of objects to compute the intent for.
    #'
    #' @return A \code{Set} with the intent.
    #'
    #' @export
    intent = function(S) {
      if (private$is_many_valued) {
        error_many_valued()
      }

      if (inherits(S, "Set")) {
        if (all(S$get_attributes() == self$objects)) {
          S <- S$get_vector()
        } else {
          S <- match_attributes(S, self$objects)
          S <- S$get_vector()
          warn <- c(
            "The attributes in the input set are not the same",
            " that in the formal context. Attempting to match",
            " attribute names gives ",
            .set_to_string(S, self$objects)
          ) |>
            stringr::str_flatten()
          warning(warn, call. = FALSE, immediate. = TRUE)
          # stop("It is not a set of the required type (set of objects).", call. = FALSE)
        }
      }

      if (length(S) == length(self$objects)) {
        R <- compute_intent(
          S,
          Matrix::as.matrix(Matrix::t(self$I)),
          connection = private$connection,
          name = private$logic
        )

        if (length(R@i) > 0) {
          # Non-empty set:
          R <- Matrix::sparseMatrix(
            i = R@i + 1,
            j = rep(1, length(R@i)),
            x = R@x,
            dims = c(length(self$attributes), 1)
          )

          R <- Set$new(
            attributes = self$attributes,
            M = R
          )
        } else {
          # Empty intent
          R <- Set$new(attributes = self$attributes)
        }

        return(R)
      } else {
        stop(
          "It is not a set of the required type (set of objects).",
          call. = FALSE
        )
      }
    },

    #' @description
    #' Get the intent of a fuzzy set of objects
    #'
    #' @param S   (\code{Set}) The set of objects to compute the intent for.
    #'
    #' @return A \code{Set} with the intent.
    #'
    #' @export
    uparrow = function(S) {
      self$intent(S)
    },

    #' @description
    #' Get the extent of a fuzzy set of attributes
    #'
    #' @param S   (\code{Set}) The set of attributes to compute the extent for.
    #'
    #' @return A \code{Set} with the intent.
    #'
    #' @export
    extent = function(S) {
      # TODO: Apply scales to Sets.

      if (private$is_many_valued) {
        error_many_valued()
      }

      if (inherits(S, "Set")) {
        if (all(S$get_attributes() == self$attributes)) {
          S <- S$get_vector()
        } else {
          S <- match_attributes(S, self$attributes)
          S <- S$get_vector()
          warn <- c(
            "The attributes in the input set are not the same",
            " that in the formal context. Attempting to match",
            " attribute names gives ",
            .set_to_string(S, self$attributes)
          ) |>
            stringr::str_flatten()
          warning(warn, call. = FALSE, immediate. = TRUE)
          # stop("It is not a set of the required type (set of attributes).", call. = FALSE)
        }
      }

      if (length(S) == length(self$attributes)) {
        R <- compute_extent(
          S,
          Matrix::as.matrix(Matrix::t(self$I)),
          connection = private$connection,
          name = private$logic
        )

        if (length(R@i) > 0) {
          # Non-empty set:
          R <- Matrix::sparseMatrix(
            i = R@i + 1,
            j = rep(1, length(R@i)),
            x = R@x,
            dims = c(length(self$objects), 1)
          )

          R <- Set$new(
            attributes = self$objects,
            M = R
          )
        } else {
          # Empty extent
          R <- Set$new(attributes = self$objects)
        }

        return(R)
      } else {
        stop(
          "It is not a set of the required type (set of objects).",
          call. = FALSE
        )
      }
    },

    #' @description
    #' Get the extent of a fuzzy set of attributes
    #'
    #' @param S   (\code{Set}) The set of attributes to compute the extent for.
    #'
    #' @return A \code{Set} with the intent.
    #'
    #' @export
    downarrow = function(S) {
      self$extent(S)
    },

    #' @description
    #' Get the closure of a fuzzy set of attributes
    #'
    #' @param S   (\code{Set}) The set of attributes to compute the closure for.
    #'
    #' @return A \code{Set} with the closure.
    #'
    #' @export
    closure = function(S) {
      if (private$is_many_valued) {
        error_many_valued()
      }

      if (inherits(S, "Set")) {
        if (all(S$get_attributes() == self$attributes)) {
          S <- S$get_vector()
        } else {
          S <- match_attributes(S, self$attributes)
          S <- S$get_vector()
          warn <- c(
            "The attributes in the input set are not the same",
            " that in the formal context. Attempting to match",
            " attribute names gives ",
            .set_to_string(S, self$attributes)
          ) |>
            stringr::str_flatten()
          warning(warn, call. = FALSE, immediate. = TRUE)

          # stop("It is not a set of the required type (set of attributes).", call. = FALSE)
        }
      }

      if (length(S) == length(self$attributes)) {
        R <- compute_closure(
          S,
          Matrix::as.matrix(Matrix::t(self$I)),
          connection = private$connection,
          name = private$logic
        )

        if (length(R@i) > 0) {
          # Non-empty set:
          R <- Matrix::sparseMatrix(
            i = R@i + 1,
            j = rep(1, length(R@i)),
            x = R@x,
            dims = c(length(self$attributes), 1)
          )

          R <- Set$new(
            attributes = self$attributes,
            M = R
          )
        } else {
          # Empty closure
          R <- Set$new(attributes = self$attributes)
        }

        return(R)
      } else {
        stop(
          "It is not a set of the required type (set of objects).",
          call. = FALSE
        )
      }
    },

    #' @description
    #' Object Concept
    #'
    #' @param object (character) Name of the object to compute its associated concept
    #'
    #' @return
    #' The object concept associated to the object given.
    #'
    #' @export
    obj_concept = function(object) {
      if (private$is_many_valued) {
        error_many_valued()
      }

      S <- Set$new(attributes = self$objects)
      S$assign(attributes = object, values = 1)

      B <- self$intent(S)
      A <- self$extent(B)

      C <- Concept$new(extent = A, intent = B)

      return(C)
    },

    #' @description
    #' Attribute Concept
    #'
    #' @param attribute (character) Name of the attribute to compute its associated concept
    #'
    #' @return
    #' The attribute concept associated to the attribute given.
    #'
    #' @export
    att_concept = function(attribute) {
      if (private$is_many_valued) {
        error_many_valued()
      }

      S <- Set$new(attributes = self$attributes)
      S$assign(attributes = attribute, values = 1)

      A <- self$extent(S)
      B <- self$intent(A)

      C <- Concept$new(extent = A, intent = B)

      return(C)
    },

    #' @description
    #' Is a Concept?
    #'
    #' @param C A \code{Concept} object
    #'
    #' @return
    #' \code{TRUE} if \code{C} is a concept.
    #'
    #' @export
    is_concept = function(C) {
      O <- C$get_extent()
      A <- C$get_intent()

      # O should be the extent of A, and A should be intent of O
      return((self$extent(A) %==% O) && (self$intent(O) %==% A))
    },

    #' @description
    #' Testing closure of attribute sets
    #'
    #' @param S A \code{Set} of attributes
    #'
    #' @return
    #' \code{TRUE} if the set \code{S} is closed in this formal context.
    #' @export
    is_closed = function(S) {
      Sc <- self$closure(S)
      return(S %==% Sc)
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
      if (private$is_many_valued) {
        error_many_valued()
      }

      # Redundant attributes
      my_I <- .clarify_matrix(
        Matrix::t(self$I),
        rows = self$objects,
        cols = self$attributes
      )

      # And redundant objects
      my_I <- .clarify_matrix(
        Matrix::t(my_I),
        rows = colnames(my_I),
        cols = self$objects
      )
      my_I <- Matrix::as.matrix(Matrix::t(my_I))

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
        stop(
          "This FormalContext is not binary. Reduction is not implemented for fuzzy contexts.",
          call. = FALSE
        )
      }

      # Make a copy with the clarified context
      fc2 <- self$clarify(TRUE)

      my_I <- Matrix::as.matrix(Matrix::t(fc2$I))

      att <- fc2$attributes

      Z <- Set$new(attributes = att)

      for (y in att) {
        R <- Set$new(attributes = fc2$objects)
        R$assign(
          attributes = fc2$objects,
          values = rep(1, length(fc2$objects))
        )

        R <- R$get_vector()

        yv <- Set$new(attributes = att)
        yv$assign(attributes = y, values = 1)
        y_down <- fc2$extent(yv)

        for (yp in setdiff(att, y)) {
          ypv <- Set$new(attributes = att)
          ypv$assign(attributes = yp, values = 1)
          yp_down <- fc2$extent(ypv)

          S <- .subset(
            y_down$get_vector(),
            yp_down$get_vector()
          )

          if (S[1]) {
            R[Matrix::which(
              yp_down$get_vector() < R
            )] <- yp_down$get_vector()[Matrix::which(yp_down$get_vector() < R)]
          }
        }

        if (!.equal_sets(R, y_down$get_vector())[1]) {
          Z$assign(attributes = y, values = 1)
        }
      }

      new_att <- Z$get_attributes()[Matrix::which(Z$get_vector() > 0)]

      idx <- match(new_att, att)

      # if (length(idx) == 1) {
      #
      #   my_I <- .extract_column(my_I, idx)
      #
      # } else {
      #
      #   my_I <- my_I[, idx]
      #
      # }

      my_I <- matrix(my_I[, idx], ncol = length(idx))

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

    #' @description
    #' Build the Standard Context
    #'
    #' @details
    #' All concepts must be previously computed.
    #'
    #' @return
    #' The standard context using the join- and meet- irreducible elements.
    #' @export
    standardize = function() {
      if (private$is_many_valued) {
        error_many_valued()
      }

      if (self$concepts$is_empty()) {
        stop("Concepts must be computed beforehand.\n", call. = FALSE)
      }

      join_irr <- self$concepts$join_irreducibles()$to_list()
      meet_irr <- self$concepts$meet_irreducibles()$to_list()

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
    #' @param method (string) The name of a method for the computation of concepts. Available options can be listed with \code{conceptRegistry$get_entries()}.
    #' @param verbose   (logical) TRUE will provide a verbose output.
    #'
    #' @return A list with all the concepts in the formal context.
    #'
    #' @export
    find_concepts = function(method = "InClose", verbose = FALSE) {
      private$check_empty()

      if (private$is_many_valued) {
        error_many_valued()
      }

      # my_I <- Matrix::as.matrix(Matrix::t(self$I))

      if (all(self$I@x == 1) && (method == "InClose")) {
        my_I <- methods::as(Matrix::t(self$I), "dgCMatrix")

        # Extracción explícita de vectores
        sp_i <- my_I@i
        sp_p <- my_I@p
        sp_dim <- my_I@Dim

        L <- InClose_Reorder(
          sp_i_sexp = sp_i,
          sp_p_sexp = sp_p,
          dim_sexp = sp_dim,
          verbose = verbose
        )

        # L <- InClose_Reorder(
        #   I = my_I,
        #   # attrs = self$attributes,
        #   verbose = verbose
        # )
      } else {
        my_I <- Matrix::as.matrix(Matrix::t(self$I))

        method <- conceptRegistry$get_entry(method[1])
        if (is.null(method)) {
          avail_methods <- conceptRegistry$get_entry_names()

          stop("Unrecognized method.")
        }

        fun <- method$fun

        if (method$method == "NextClosure") {
          grades_set <- rep(list(self$grades_set), length(self$attributes))
        } else {
          grades_set <- self$grades_set
        }

        attrs <- self$attributes

        if (verbose) {
          glue::glue("You have chosen {method$method}\n") |>
            cli::cat_line()
        }

        # L <- next_closure_concepts(
        L <- method$fun(
          I = my_I,
          grades_set = grades_set,
          attrs = attrs,
          connection = private$connection,
          name = private$logic,
          verbose = verbose
        )
      }

      # Since the previous function gives the list of intents of
      # the computed concepts, now we will compute the corresponding
      # extents.

      if (length(self$attributes) == 1) {
        my_intents <- Matrix::Matrix(t(as.vector(L$intents)), sparse = TRUE)

        my_extents <- Matrix::Matrix(t(as.vector(L$extents)), sparse = TRUE)
      } else {
        my_intents <- L$intents

        my_extents <- L$extents
      }

      self$concepts <- ConceptLattice$new(
        extents = my_extents,
        intents = my_intents,
        objects = self$objects,
        attributes = self$attributes,
        I = self$I
      )

      if (verbose) {
        cat("Number of closures", L$closure_count, "\n")
      }

      # return(invisible(self$concepts))
      return(invisible(self))
    },

    #' @description
    #' Use modified Ganter algorithm to compute both concepts and implications
    #'
    #' @param save_concepts (logical) \code{TRUE} will also compute and save the concept lattice. \code{FALSE} is usually faster, since it only computes implications.
    #' @param verbose   (logical) \code{TRUE} will provide a verbose output.
    #'
    #' @return Nothing, just updates the internal fields \code{concepts} and \code{implications}.
    #'
    #'
    #' @export
    find_implications = function(save_concepts = TRUE, verbose = FALSE) {
      private$check_empty()

      if (private$is_many_valued) {
        error_many_valued()
      }

      if (all(self$I@x == 1)) {
        I <- self$incidence()
        mode(I) <- "integer"
        L <- binary_next_closure_implications(
          I,
          verbose = verbose
        )
      } else {
        my_I <- Matrix::as.matrix(Matrix::t(self$I))
        grades_set <- rep(list(self$grades_set), length(self$attributes))
        attrs <- self$attributes

        L <- next_closure_implications(
          I = my_I,
          grades_set = grades_set,
          attrs = attrs,
          save_concepts = save_concepts,
          connection = private$connection,
          name = private$logic,
          verbose = verbose
        )
      }

      if (
        !is.null(private$bg_implications) &&
          private$bg_implications$cardinality() > 0
      ) {
        private$bg_implications <- reorder_attributes(
          private$bg_implications,
          self$attributes
        )

        bg <- private$bg_implications$clone()
        suppressMessages(bg$apply_rules(c("simp", "rsimp")))

        # print(private$bg_implications %~% bg)
        lhs_bg <- bg$get_LHS_matrix()
        rhs_bg <- bg$get_RHS_matrix()
        L1 <- complete_rhs(LHS = lhs_bg, RHS = rhs_bg)

        lhs_bg <- L1$lhs
        rhs_bg <- L1$rhs

        L2 <- .imp_to_basis_bg(
          lhs_bg = lhs_bg,
          rhs_bg = rhs_bg,
          LHS = L$LHS,
          RHS = L$RHS,
          attributes = self$attributes
        )
        L3 <- .clean(L2$lhs, L2$rhs)
        L$LHS <- L3$lhs
        L$RHS <- L3$rhs
      }

      # Since the previous function gives the list of intents of
      # the computed concepts, now we will compute the corresponding
      # extents.
      if (save_concepts) {
        my_intents <- L$concepts
        my_extents <- L$extents
      }

      if (save_concepts) {
        self$concepts <- ConceptLattice$new(
          extents = my_extents,
          intents = my_intents,
          objects = self$objects,
          attributes = self$attributes,
          I = self$I
        )
      }

      # Now, add the computed implications
      # TODO: check what happens with only 1 implication.
      if (ncol(L$LHS) > 0) {
        # There are implications (the first one is dummy
        # emptyset -> emptyset )
        my_LHS <- convert_to_sparse(L$LHS)
        my_RHS <- convert_to_sparse(L$RHS)

        extracted_implications <- ImplicationSet$new(
          attributes = self$attributes,
          lhs = my_LHS,
          rhs = my_RHS,
          I = self$I
        )
      } else {
        extracted_implications <- ImplicationSet$new(
          attributes = self$attributes,
          I = self$I
        )
      }

      self$implications <- extracted_implications
      self$implications$use_logic(self$get_logic())

      return(invisible(self))
    },

    #' @description
    #' Find causal rules
    #'
    #' @param response_var (character) The name of the response variable.
    #' @param min_support (numeric) Minimum support for the premise attributes.
    #' @param confidence_level (numeric) Confidence level for the causality test.
    #' @param max_length (integer) Maximum length of the premise.
    #' @param verbose (logical) Show verbose output.
    #'
    #' @return A \code{RuleSet} object containing the discovered causal rules and their quality metrics.
    #' @export
    find_causal_rules = function(
      response_var,
      min_support = 0.1,
      confidence_level = 0.95,
      max_length = 3,
      verbose = FALSE
    ) {
      df <- find_causal_rules(
        self,
        response_var,
        min_support,
        confidence_level,
        max_length,
        verbose
      )

      if (nrow(df) == 0) {
        return(RuleSet$new(attributes = self$attributes))
      }

      # Convert data frame to RuleSet
      n_rules <- nrow(df)
      n_attrs <- length(self$attributes)

      # Create sparse matrices
      # Using triplet format for efficiency if many rules, but dgCMatrix construction is fine
      lhs_i <- integer(0)
      lhs_j <- integer(0)

      rhs_i <- integer(0)
      rhs_j <- integer(0)

      for (k in seq_len(n_rules)) {
        # Parse premise
        if (df$premise[k] != "") {
          p_attrs <- unlist(strsplit(df$premise[k], ", "))
          p_idx <- match(p_attrs, self$attributes)
          lhs_i <- c(lhs_i, p_idx)
          lhs_j <- c(lhs_j, rep(k, length(p_idx)))
        }

        # Conclusion
        c_attrs <- df$conclusion[k]
        c_idx <- match(c_attrs, self$attributes)
        rhs_i <- c(rhs_i, c_idx)
        rhs_j <- c(rhs_j, rep(k, length(c_idx)))
      }

      lhs_mat <- Matrix::sparseMatrix(
        i = lhs_i,
        j = lhs_j,
        dims = c(n_attrs, n_rules)
      )
      rhs_mat <- Matrix::sparseMatrix(
        i = rhs_i,
        j = rhs_j,
        dims = c(n_attrs, n_rules)
      )

      qual <- df[, setdiff(names(df), c("premise", "conclusion")), drop = FALSE]

      rs <- RuleSet$new(
        attributes = self$attributes,
        lhs = lhs_mat,
        rhs = rhs_mat,
        quality = qual
      )

      return(rs)
    },

    #' @description
    #' Factorize the formal context using Boolean/Fuzzy Matrix Factorization algorithms.
    #'
    #' @param method (character) The algorithm to use. Currently supported: "GreConD", "ASSO".
    #' @param ... Additional arguments:
    #' \itemize{
    #'   \item For \code{GreConD}: \code{w} (weight, default 1.0), \code{stop_threshold_ratio} (error tolerance, default 0.0).
    #'   \item For \code{ASSO}: \code{threshold} (confidence threshold, default 0.7), \code{w_pos} (reward), \code{w_neg} (penalty).
    #' }
    #'
    #' @return A list with two \code{FormalContext} objects:
    #' \itemize{
    #'   \item \code{object_factor}: The context mapping Objects to Factors (Matrix A).
    #'   \item \code{factor_attribute}: The context mapping Factors to Attributes (Matrix B).
    #' }
    #' @export
    factorize = function(method = "GreConD", ...) {
      if (self$is_empty()) {
        stop("Context is empty.")
      }

      # Convertir a densa para algoritmos numéricos complejos
      I_mat <- as.matrix(self$incidence())

      dots <- list(...)
      factors_list <- NULL

      if (method == "GreConD" || method == "GreConD+") {
        w <- ifelse(is.null(dots$w), 1.0, dots$w)
        stop_ratio <- ifelse(
          is.null(dots$stop_threshold_ratio),
          0.0,
          dots$stop_threshold_ratio
        )

        # --- INTEGRACIÓN DIFUSA ---
        # Recuperamos la lógica actual del objeto
        current_logic <- self$get_logic()

        factors_list <- grecond_plus_cpp(I_mat, w, stop_ratio, current_logic)
      } else if (method == "ASSO") {
        threshold <- ifelse(is.null(dots$threshold), 0.7, dots$threshold)
        w_pos <- ifelse(is.null(dots$w_pos), 1.0, dots$w_pos)
        w_neg <- ifelse(is.null(dots$w_neg), 1.0, dots$w_neg)

        factors_list <- asso_cpp(I_mat, threshold, w_pos, w_neg)
      } else {
        stop(paste("Unknown factorization method:", method))
      }

      if (length(factors_list) == 0) {
        warning("No factors found.")
        return(NULL)
      }

      # Reconstruir Contextos de Factores
      n_factors <- length(factors_list)
      factor_names <- paste0("F", seq_len(n_factors))

      A <- matrix(0, nrow = length(self$objects), ncol = n_factors)
      rownames(A) <- self$objects
      colnames(A) <- factor_names

      B <- matrix(0, nrow = n_factors, ncol = length(self$attributes))
      rownames(B) <- factor_names
      colnames(B) <- self$attributes

      for (k in seq_len(n_factors)) {
        f <- factors_list[[k]]
        A[, k] <- f$extent
        B[k, ] <- f$intent
      }

      # Crear nuevos objetos con la MISMA configuración de lógica que el padre
      ctx_A <- FormalContext$new(A)
      ctx_B <- FormalContext$new(B)

      ctx_A$use_logic(self$get_logic())
      ctx_B$use_logic(self$get_logic())

      return(list(
        object_factor = ctx_A,
        factor_attribute = ctx_B
      ))
    },

    #' @description
    #' Convert the formal context to object of class \code{transactions} from the \code{arules} package
    #'
    #' @return A \code{transactions} object.
    #'
    #' @export
    to_transactions = function() {
      check_needed_pkg("arules", "exporting to transactions format")

      private$check_empty()

      if (private$is_many_valued) {
        error_many_valued()
      }

      return(methods::as(methods::as(self$I, "nMatrix"), "transactions"))
      # return(to_transactions.SpM(self$I))
    },

    #' @description
    #' Save a \code{FormalContext} to RDS or CXT format
    #'
    #' @param filename   (character) Path of the  file where to store the \code{FormalContext}.
    #'
    #' @return Invisibly the current \code{FormalContext}.
    #'
    #' @details The format is inferred from the extension of the filename.
    #'
    #' @export
    save = function(filename = tempfile(fileext = ".rds")) {
      private$check_empty()

      pattern <- "(?<!^|[.]|/)[.]([^.]+)$"

      extension <- filename |>
        stringr::str_extract_all(pattern) |>
        unlist() |>
        tolower()

      if (extension == ".cxt") {
        if (!private$is_many_valued) {
          to_cxt(
            I = self$incidence(),
            objects = self$objects,
            attributes = self$attributes,
            filename
          )
        }

        return(invisible(self))
      }

      if (!self$concepts$is_empty()) {
        extents <- self$concepts$extents()
        intents <- self$concepts$intents()
      } else {
        extents <- NULL
        intents <- NULL
      }

      if (private$is_many_valued) {
        L <- list(
          I = self$I,
          many_valued_I = private$many_valued_I,
          extents = NULL,
          intents = NULL,
          attributes = self$attributes,
          objects = self$objects,
          expanded_grades_set = NULL,
          grades_set = NULL,
          implications = self$implications
        )
      } else {
        L <- list(
          I = self$I,
          extents = extents,
          intents = intents,
          attributes = self$attributes,
          objects = self$objects,
          expanded_grades_set = self$expanded_grades_set,
          grades_set = self$grades_set,
          implications = self$implications
        )
      }

      saveRDS(L, file = filename)

      return(invisible(self))
    },

    #' @description
    #' Load a \code{FormalContext} from a file
    #'
    #' @param filename   (character) Path of the file to load the \code{FormalContext} from.
    #'
    #' @details Currently, only RDS, CSV and CXT files are supported.
    #'
    #' @return The loaded \code{FormalContext}.
    #'
    #' @export
    load = function(filename) {
      pattern <- "(?<!^|[.]|/)[.]([^.]+)$"

      extension <- filename |>
        stringr::str_extract_all(pattern) |>
        unlist() |>
        tolower()

      if (extension == ".rds") {
        L <- readRDS(filename)

        if ("many_valued_I" %in% names(L)) {
          self$initialize(L$many_valued_I)

          return()
        }

        self$I <- L$I
        self$attributes <- L$attributes
        self$objects <- L$objects
        self$expanded_grades_set <- L$expanded_grades_set
        self$grades_set <- L$grades_set
        self$implications <- ImplicationSet$new(
          attributes = L$attributes,
          I = L$I,
          lhs = L$implications$get_LHS_matrix(),
          rhs = L$implications$get_RHS_matrix()
        )

        if (!is.null(L$extents)) {
          self$concepts <- ConceptLattice$new(
            extents = L$extents,
            intents = L$intents,
            objects = L$objects,
            attributes = L$attributes,
            I = L$I
          )
        }
      }

      if (extension == ".csv") {
        I <- read.csv(filename)

        if (is.character(I[[1]]) || is.factor(I[[1]])) {
          objects <- as.character(I[[1]])
          I <- I[, -1]
          rownames(I) <- objects
        }

        I <- as.matrix(I)

        self$initialize(I)
      }

      if (extension == ".cxt") {
        txt <- readLines(filename)

        n_objects <- txt[3] |> as.numeric()
        n_attributes <- txt[4] |> as.numeric()

        obj_idx <- seq(6, 6 + n_objects - 1)
        att_idx <- seq(
          6 + n_objects,
          6 + n_objects + n_attributes - 1
        )
        matrix_idx <- seq(
          6 + n_objects + n_attributes,
          6 + 2 * n_objects + n_attributes - 1
        )

        objects <- txt[obj_idx]
        attributes <- txt[att_idx]
        matrix <- txt[matrix_idx] |>
          stringr::str_replace_all(
            pattern = "[X|x]",
            replacement = "1"
          ) |>
          stringr::str_replace_all(
            pattern = stringr::fixed("."),
            replacement = "0"
          ) |>
          as.list() |>
          stringr::str_split(pattern = "") |>
          purrr::map(as.numeric)

        I <- do.call(rbind, matrix)
        rownames(I) <- objects
        colnames(I) <- attributes

        self$initialize(I)
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
    #' Export the formal context to JSON
    #'
    #' @param file (character) The path of the file to save the JSON to.
    #' @return A JSON string representing the formal context.
    #' @export
    to_json = function(file = NULL) {
      check_needed_pkg("jsonlite", "exporting to JSON")

      # We export the transpose of I so that it is row-major (Objects x Attributes)

      # Helper to convert sparse matrix
      mat_to_list <- function(M) {
        if (is.null(M)) {
          warning("mat_to_list received NULL matrix")
          return(list(
            indices = integer(0),
            columns = integer(0),
            values = numeric(0)
          ))
        }

        # Use summary to get triplets (works for dgCMatrix)
        s <- summary(M)

        # If summary returns a table-like object (e.g. for ddiMatrix), force conversion
        if (!all(c("i", "j", "x") %in% names(s))) {
          # Fallback: try to convert to TsparseMatrix if summary didn't give triplets
          T <- as(M, "TsparseMatrix")
          s <- summary(T)
        }

        if (nrow(s) == 0) {
          return(list(
            indices = integer(0),
            columns = integer(0),
            values = numeric(0)
          ))
        }

        # Convert to 0-based indexing for JSON
        res <- list(
          indices = as.integer(s$j) - 1L,
          columns = as.integer(s$i) - 1L,
          values = as.numeric(s$x)
        )
        return(res)
      }

      tr_I <- Matrix::t(self$I)

      I_data <- mat_to_list(tr_I)

      # Concepts
      concepts_data <- NULL
      if (inherits(self$concepts, "ConceptLattice")) {
        concepts_data <- self$concepts$to_json(return_list = TRUE)
      }

      # Implications
      implications_data <- NULL
      if (inherits(self$implications, "ImplicationSet")) {
        implications_data <- self$implications$to_json(return_list = TRUE)
      }

      context_data <- list(
        type = "FormalContext",
        objects = self$objects,
        attributes = self$attributes,
        I = I_data,
        grades_set = self$grades_set,
        description = self$description,
        is_many_valued = private$is_many_valued,
        concepts = concepts_data,
        implications = implications_data
      )

      json_string <- jsonlite::toJSON(context_data, auto_unbox = TRUE)

      if (!is.null(file)) {
        writeLines(json_string, file)
        return(invisible(json_string))
      } else {
        return(json_string)
      }
    },

    #' @description
    #' Prints the formal context
    #'
    #' @return Prints information regarding the formal context.
    #' @export
    print = function() {
      if (self$is_empty()) {
        cat("Empty FormalContext.\n")

        return(invisible(self))
      }

      dims <- self$dim()

      if (!private$is_many_valued) {
        I <- Matrix::as.matrix(Matrix::t(self$I))

        if (private$is_binary) {
          I <- .print_binary(I, latex = FALSE)
        }

        objects <- self$objects
        if (nrow(I) > 10) {
          I <- I[1:10, ]
          objects <- objects[1:10]
        }

        matp <- .print_matrix(
          I,
          objects = objects,
          attributes = self$attributes
        )
        M <- matp$mat
        ids <- matp$att_id
        last_attribute <- max(ids) - 1

        str <- paste0(
          "FormalContext with ",
          dims[1],
          " objects and ",
          dims[2],
          " attributes."
        ) |>
          stringr::str_wrap(width = getOption("width"))

        cat(str)
        cat("\n")

        cat(M)
        cat("\n")

        if (last_attribute < length(self$attributes)) {
          remaining <- self$attributes[-seq(last_attribute)]

          if (length(remaining) > 6) {
            remaining <- c(remaining[1:6], "...")
          }

          remaining <- remaining |>
            stringr::str_flatten(", ")

          str <- paste0("Other attributes are: ", remaining) |>
            stringr::str_wrap(width = getOption("width"))

          cat(str, "\n")
        }
      } else {
        str <- paste0(
          "FormalContext with ",
          dims[1],
          " objects and ",
          dims[2],
          " attributes."
        ) |>
          stringr::str_wrap(width = getOption("width"))

        cat(str)
        cat("\n")

        print_many_valued(private$many_valued_I)

        # print(tibble::tibble(private$many_valued_I))
      }
    },

    #' @description
    #' Write the context in LaTeX format
    #'
    #' @param table (logical) If \code{TRUE}, surrounds everything between \code{\\begin{table}} and \code{\\end{table}}.
    #' @param label (character) The label for the table environment.
    #' @param caption (character) The caption of the table.
    #' @param fraction (character) If \code{none}, no fractions are produced. Otherwise, if it is \code{frac}, \code{dfrac} or \code{sfrac}, decimal numbers are represented as fractions with the corresponding LaTeX typesetting.
    #'
    #' @return
    #' A table environment in LaTeX.
    #'
    #' @export
    #' @importFrom glue glue
    #'
    to_latex = function(table = TRUE, label = "", caption = "") {
      # TODO: export a many-valued context to LaTeX
      if (private$is_many_valued) {
        error_many_valued()
      }

      fraction <- fcaR_options("latex_fraction")

      I <- Matrix::as.matrix(Matrix::t(self$I))

      if (private$is_binary) {
        I <- .print_binary(I, latex = TRUE)
      } else {
        if (fraction != "none") {
          I <- .to_fraction(I, latex = TRUE, type = fraction)
        } else {
          decimal_places <- fcaR_options("decimal_places")
          I[] <- I |>
            formatC(digits = decimal_places) |>
            stringr::str_replace_all("\\s*", "")
        }
      }

      if (fcaR_options("use_mathrm")) {
        objects <- glue::glue("\\mathrm{{{self$objects}}}")
        attributes <- glue::glue("\\mathrm{{{self$attributes}}}")
      } else {
        objects <- self$objects
        attributes <- self$attributes
      }

      str <- context_to_latex(
        I,
        objects = objects,
        attributes = attributes
      )

      if (table) {
        my_caption <- glue::glue(
          "\\caption{{{caption}}}\\label{{{label}}}"
        )
        # my_caption <- paste0("\\caption{,
        #                      \\label{",
        #                      label, "}",
        #                      caption, "}")

        str <- c(
          "\\begin{table}",
          my_caption,
          "\\centering",
          str,
          "\\end{table}"
        )
      }

      cat(str)

      return(invisible(str))
    },

    #' @description
    #' Incidence matrix of the formal context
    #'
    #' @return The incidence matrix of the formal context
    #' @export
    #'
    #' @examples
    #' fc <- FormalContext$new(planets)
    #' fc$incidence()
    incidence = function() {
      if (private$is_many_valued) {
        return(private$many_valued_I)
      } else {
        I <- Matrix::as.matrix(Matrix::t(self$I))
        dimnames(I) <- list(self$objects, self$attributes)

        return(I)
      }
    },

    #' @description
    #' Generates a new FormalContext restricted to a subset of objects and/or attributes.
    #'
    #' @param objects (character or integer vector) The names or indices of the objects to keep. If NULL, keeps all.
    #' @param attributes (character or integer vector) The names or indices of the attributes to keep. If NULL, keeps all.
    #'
    #' @return A new \code{FormalContext} object representing the subcontext.
    #'
    #' @export
    #'
    #' @examples
    #' fc <- FormalContext$new(planets)
    #' fc$subcontext(attributes = c("moon", "no_moon"))
    subcontext = function(objects, attributes) {
      if (missing(objects)) {
        objects <- self$objects
      }
      if (missing(attributes)) {
        attributes <- self$attributes
      }

      # --- 1. Resolver Índices de Objetos ---
      if (is.null(objects)) {
        # Si es NULL, mantenemos todos los índices
        idx_objs <- seq_len(self$dim()[1])
      } else if (is.character(objects)) {
        # Si son nombres, buscamos sus índices
        idx_objs <- match(objects, self$objects)
        # Filtramos NAs (nombres que no existen)
        idx_objs <- idx_objs[!is.na(idx_objs)]
      } else {
        # Asumimos numérico/lógico
        idx_objs <- objects
      }

      # --- 2. Resolver Índices de Atributos ---
      if (is.null(attributes)) {
        idx_attrs <- seq_len(self$dim()[2])
      } else if (is.character(attributes)) {
        idx_attrs <- match(attributes, self$attributes)
        idx_attrs <- idx_attrs[!is.na(idx_attrs)]
      } else {
        idx_attrs <- attributes
      }

      # --- 3. Subconjunto de la Matriz (CRÍTICO) ---
      I_sub <- self$incidence()[idx_objs, idx_attrs, drop = FALSE]
      # rownames(I_sub) <- self$objects[idx_objs]
      # rownames(I_sub) <- self$attributes[idx_attrs]

      # --- 4. Crear el Nuevo Contexto ---
      # FormalContext$new es capaz de ingerir una matriz dispersa directamente.
      # Esto reinicia automáticamente conceptos e implicaciones a NULL (lo correcto).
      new_fc <- FormalContext$new(I_sub)

      # --- 5. Preservar Metadatos Vitales ---
      # El subcontexto debe operar bajo las mismas reglas matemáticas que el padre.
      # Copiamos la lógica difusa actual.
      new_fc$use_logic(private$logic)

      # (Opcional) Si hubiera Background Knowledge cargado, ¿debería copiarse?
      # Generalmente no, porque las escalas dependen de los atributos originales.
      # Si se han borrado atributos, el BG knowledge podría ser inválido.
      # Por seguridad, es mejor dejarlo limpio.

      return(new_fc)
    },
    # subcontext = function(objects,
    #                       attributes) {
    #   if (missing(objects)) objects <- self$objects
    #   if (missing(attributes)) attributes <- self$attributes
    #
    #   if (is.numeric(objects)) {
    #
    #     final_objects <- self$objects[objects[objects > 0 & objects < length(self$objects)]]
    #   } else {
    #     final_objects <- intersect(objects, self$objects)
    #   }
    #
    #   if (is.numeric(attributes)) {
    #     final_attributes <- self$attributes[attributes[attributes > 0 & attributes < length(self$attributes)]]
    #   } else {
    #     final_attributes <- intersect(attributes, self$attributes)
    #   }
    #
    #   if (length(objects) == 0) objects <- self$objects
    #   if (length(attributes) == 0) attributes <- self$attributes
    #
    #   I <- self$incidence()[final_objects, final_attributes]
    #   I <- matrix(I,
    #     nrow = length(final_objects),
    #     ncol = length(final_attributes)
    #   )
    #   rownames(I) <- final_objects
    #   colnames(I) <- final_attributes
    #
    #   return(FormalContext$new(I))
    # },

    #' @description
    #' Subcontext of the formal context
    #'
    #' @param objects (character array) Name of the objects to
    #' keep.
    #' @param attributes (character array) Names of the attributes
    #' to keep.
    #'
    #' @details
    #' A warning will be issued if any of the names is not present
    #' in the list of objects or attributes of the formal context.
    #'
    #' If \code{objects} or \code{attributes} is empty, then it is
    #' assumed to represent the whole set of objects or attributes
    #' of the original formal context.
    #'
    #' @return Another \code{FormalContext} that is a subcontext
    #' of the original one, with only the objects and attributes
    #' selected.
    #' @export
    #'
    #' @examples
    #' fc <- FormalContext$new(planets)
    #' fc[, c("moon", "no_moon")]
    `[` = function(objects, attributes) {
      self$subcontext(objects, attributes)
    },

    #' @description
    #' Plot the formal context table
    #'
    #' @param to_latex      (logical) If \code{TRUE}, export the plot as a \code{tikzpicture} environment that can be included in a \code{LaTeX} file.
    #' @param ...          Other parameters to be passed to the \code{tikzDevice} that renders the lattice in \code{LaTeX}, or for the figure caption. See \code{Details}.
    #'
    #' @details
    #' Particular parameters that control the size of the \code{tikz} output are: \code{width}, \code{height} (both in inches), and \code{pointsize} (in points), that should be set to the font size used in the \code{documentclass} header in the \code{LaTeX} file where the code is to be inserted.
    #'
    #' If a \code{caption} is provided, the whole \code{tikz} picture will be wrapped by a \code{figure} environment and the caption set.
    #'
    #' @return If \code{to_latex} is \code{FALSE}, it returns nothing, just plots the graph of the formal context. Otherwise, this function returns the \code{LaTeX} code to reproduce the formal context plot.
    #'
    #' @export
    plot = function(to_latex = FALSE, ...) {
      if (private$is_many_valued) {
        error_many_valued()
      }

      private$check_empty()
      if (!private$can_plot) {
        warning(
          "The R system has not the needed capabilities to plot.",
          call. = FALSE
        )
        return(invisible(FALSE))
      }

      plot_context(self$I, to_latex, ...)
    },

    #' @description
    #' Sets the logic to use
    #'
    #'
    #' @param name The name of the logic to use. To see the available names, run \code{available_logics()}.
    #'
    #' @export
    use_logic = function(name = available_logics()) {
      name <- match.arg(name)
      if (name %in% available_logics()) {
        private$logic <- name
      }
    },

    #' @description
    #' Gets the logic used
    #'
    #' @return A string with the name of the logic.
    #'
    #' @export
    get_logic = function() {
      private$logic
    },

    #' @description
    #' Sets the name of the Galois connection to use
    #'
    #' @param connection The name of the Galois connection. Available connections are "standard" (antitone), "benevolent1" and "benevolent2" (isotone)
    #'
    #' @export
    use_connection = function(connection) {
      if (connection %in% c("standard", "benevolent1", "benevolent2")) {
        private$connection <- connection
      }
    },

    #' @description
    #' Gets the name of the Galois connection
    #'
    #' @return A string with the name of the Galois connection
    #'
    #' @export
    get_connection = function() {
      private$connection
    }
  ),
  private = list(
    logic = "Zadeh",
    connection = "standard",
    is_binary = FALSE,
    is_many_valued = FALSE,
    can_plot = TRUE,
    many_valued_I = NULL,
    scales = list(),
    bg_implications = NULL,
    bg_implications_basis = NULL,
    check_empty = function() {
      if (self$is_empty()) {
        stop(
          "The formal context is empty. The only allowed method is 'load' to import from a previously saved RDS file.",
          call. = FALSE
        )
      }

      return(invisible(NULL))
    }
  )
)

#' @title Import FormalContext from JSON
#' @description Reconstructs a FormalContext object from a JSON string.
#' @param json_str A JSON string generated by \code{to_json()}.
#' @return A \code{FormalContext} object.
#' @export
context_from_json <- function(json_str) {
  check_needed_pkg("jsonlite", "import from JSON")

  data <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)

  if (data$type != "FormalContext") {
    stop("Invalid JSON: type must be 'FormalContext'")
  }

  objects <- unlist(data$objects)
  attributes <- unlist(data$attributes)

  # Reconstruct I
  i <- unlist(data$I$columns)
  j <- unlist(data$I$indices)
  x <- unlist(data$I$values)

  if (length(i) > 0) {
    I <- Matrix::sparseMatrix(
      i = i + 1,
      j = j + 1,
      x = x,
      dims = c(length(objects), length(attributes)),
      dimnames = list(objects, attributes)
    )
  } else {
    I <- Matrix::Matrix(
      0,
      nrow = length(objects),
      ncol = length(attributes),
      sparse = TRUE
    )
    dimnames(I) <- list(objects, attributes)
  }

  fc <- FormalContext$new(I)

  # Restore other fields
  if (!is.null(data$grades_set)) {
    fc$grades_set <- unlist(data$grades_set)
  }
  if (!is.null(data$description)) {
    fc$description <- unlist(data$description)
  }

  # Restore Concepts
  if (!is.null(data$concepts)) {
    # Reconstruct lattice from list data
    # We can invoke lattice_from_json by passing the sub-json
    # parsing sub-json from data object is tricky as we have list
    # Use toJSON to get string? Or refactor lattice_from_json to accept list?
    # Refactoring lattice_from_json to accept list is cleaner.
    # But for now, convert list to JSON string is easier.

    concepts_json <- jsonlite::toJSON(data$concepts, auto_unbox = TRUE)
    cl <- lattice_from_json(concepts_json)
    fc$concepts <- cl
  }

  # Restore Implications
  if (!is.null(data$implications)) {
    implications_json <- jsonlite::toJSON(data$implications, auto_unbox = TRUE)
    imp <- implications_from_json(implications_json)
    fc$implications <- imp
  }

  return(fc)
}
