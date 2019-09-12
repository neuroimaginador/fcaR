#' @export
#' @import scales RColorBrewer
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

    # Object constructor
    initialize = function(I,
                          grades_set = sort(unique(as.vector(I)))) {

      # Transform the formal context to sparse
      if (inherits(I, "transactions")) {

        attributes <- I@itemInfo$labels
        I <- as(I@data, "dgCMatrix")
        objects <- paste0(seq(ncol(I)))

      } else {

        attributes <- colnames(I)
        objects <- rownames(I)

        constant_cols <- which(apply(I, 2, max) == apply(I, 2, min))

        if (length(constant_cols) > 0) {

          message(paste0("Removed constant columns: ", str_flatten(attributes[constant_cols], collapse = ", ")))

          I <- I[, -constant_cols]
          attributes <- attributes[-constant_cols]

        }

        I <- as(Matrix(t(I),
                       sparse = TRUE), "dgCMatrix")

      }

      expanded_grades_set <- compute_grades(t(I))

      self$I <- I
      self$grades_set <- grades_set
      self$expanded_grades_set <- expanded_grades_set
      self$objects <- colnames(I)
      self$attributes <- attributes

      # lengths <- sapply(expanded_grades_set, length)
      # o <- order(lengths, decreasing = TRUE)
      #
      # self$I <- I[o, ]
      # self$grades_set <- grades_set
      # self$expanded_grades_set <- expanded_grades_set[o]
      #
      # self$objects <- colnames(I)
      # self$attributes <- attributes[o]

    },

    # Add a precomputed implication set
    add_implications = function(impl_set) {

      if (inherits(impl_set, "rules")) {

        # If it comes from arules
        # convert to our format
        implications <- implication_set$new()
        implications$from_arules(impl_set)

        # self$implications <- implications$clone()

      } else {

        # If it's already an implication set
        # just clone it
        implications <- impl_set$clone()

      }

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

    # Use Ganter Algorithm to compute concepts
    compute_concepts = function(verbose = FALSE) {

      if (!is.null(self$concepts)) return(self$concepts)

      self$concepts <- .get_fuzzy_concepts_sparse(as.matrix(t(self$I)),
                                                  self$grades_set,
                                                  verbose = verbose)

      return(self$concepts)

    },

    # Use modified Ganter algorithm to compute both
    # concepts and implications
    extract_implications_concepts = function(sort = c("none", "decreasing", "increasing"),
                                             verbose = FALSE) {

      # on.exit({
      #
      #   DGbasis <- get("DGbasis", envir = globalenv())
      #   self$implications <- DGbasis$clone()
      #   rm("DGbasis", envir = globalenv())
      #
      # })
      #
      sort <- match.arg(sort,
                        choices = c("none", "decreasing", "increasing"))

      lengths <- sapply(self$expanded_grades_set, length)
      o <- order(lengths, decreasing = TRUE)

      o <- switch(tolower(sort),
                  "none" = seq_along(self$attributes),
                  "decreasing" = o,
                  "increasing" = rev(o))

      my_I <- as.matrix(t(self$I))[, o]
      grades_set <- self$expanded_grades_set[o]
      attrs <- self$attributes[o]

      L <- ganters_algorithm_implications_tree_final(I = my_I,
                                                     grades_set = grades_set,
                                                     attrs = attrs)

      # Undo the permutation
      v <- order(o)
      my_concepts <- L$concepts[v, ]
      my_LHS <- L$LHS[v, -1]
      my_RHS <- L$RHS[v, -1]

      if (is.null(self$concepts)) {

        self$concepts <- my_concepts

      } else {

        self$concepts <- cbind(self$concepts, my_concepts)

      }

      extracted_implications <- implication_set$new(attributes = self$attributes,
                                                    lhs = my_LHS,
                                                    rhs = my_RHS)

      if (is.null(self$implications)) {

        self$implications <- extracted_implications

      } else {

        self$add_implications(extracted_implications)

      }

    },

    # extract_implications = function(verbose = FALSE) {
    #
    #   remove_from_fca_env(implications)
    #
    #   on.exit({
    #
    #     read_from_fca_env(implications)
    #
    #     if (!is.null(implications)) {
    #
    #       self$implications <- implications$clone()
    #
    #       # remove_from_fca_env(implications)
    #
    #     }
    #
    #   })
    #
    #   tmp <- .get_implications_in_binary(as.matrix(t(self$I)), verbose = verbose)
    #
    #
    #   self$implications <- tmp # implications
    #
    # },

    run_arm = function(type = "apriori",
                       parameter = NULL,
                       appearance = NULL,
                       control = NULL) {

      I <- as.matrix(t(self$I))

      grades_set <- sort(unique(as.vector(I)))
      grades_set <- grades_set[grades_set > 0]

      my_I <- .expand_dataset(I,
                              grades_set,
                              implications = FALSE)
      binaries <- my_I$binaries

      my_transactions <- as(my_I$I, "transactions")

      r <- switch(tolower(type),

                  "apriori" = {

                    parameter <- c(parameter, conf = 1)
                    parameter <- as.list(parameter)
                    apriori(my_transactions,
                            parameter = parameter,
                            appearance = appearance,
                            control = control)

                  },

                  "eclat" = {

                    itemsets <- eclat(my_transactions,
                                      parameter = parameter,
                                      control = control)

                    ruleInduction(itemsets,
                                  my_transactions,
                                  confidence = 1)

                  }

      )

      # r <- apriori(as(my_I$I, "transactions"), parameter = c(conf = 1, list(...)))

      r <- r[!is.redundant(r)]

      LHS <- t(as.matrix(r@lhs@data))
      RHS <- t(as.matrix(r@rhs@data))

      LHS <- .recode_to_original_grades(LHS,
                                        grades_set,
                                        binaries = binaries)
      RHS <- .recode_to_original_grades(RHS,
                                        grades_set,
                                        binaries = binaries)

      LHS <- t(Matrix(LHS, sparse = TRUE))
      RHS <- t(Matrix(RHS, sparse = TRUE))

      self$implications <- implication_set$new(name = "apriori",
                                               attributes = self$attributes,
                                               lhs = LHS,
                                               rhs = RHS)

    },

    # run_apriori = function(...) {
    #
    #   I <- as.matrix(t(self$I))
    #
    #   grades_set <- sort(unique(as.vector(I)))
    #   grades_set <- grades_set[grades_set > 0]
    #
    #   my_I <- .expand_dataset(I,
    #                           grades_set,
    #                           implications = FALSE)
    #   binaries <- my_I$binaries
    #
    #   r <- apriori(as(my_I$I, "transactions"), parameter = c(conf = 1, list(...)))
    #
    #   r <- r[!is.redundant(r)]
    #
    #   LHS <- t(as.matrix(r@lhs@data))
    #   RHS <- t(as.matrix(r@rhs@data))
    #
    #   LHS <- .recode_to_original_grades(LHS,
    #                                     grades_set,
    #                                     binaries = binaries)
    #   RHS <- .recode_to_original_grades(RHS,
    #                                     grades_set,
    #                                     binaries = binaries)
    #
    #   LHS <- t(Matrix(LHS, sparse = TRUE))
    #   RHS <- t(Matrix(RHS, sparse = TRUE))
    #
    #   self$implications <- implication_set$new(name = "apriori",
    #                                            attributes = self$attributes,
    #                                            lhs = LHS,
    #                                            rhs = RHS)
    #
    # },

    convert_implications_to_arules = function(quality = TRUE) {

      R <- self$implications$to_arules()

      if (quality) {

        quality(R) <- interestMeasure(R,
                                      transactions = as(as(self$I, "ngCMatrix"), "transactions"))

      }

      return(R)

    },

    # Plot the concept lattice
    plot_lattice = function() {

      if (length(self$concepts) > 0) {

        .draw_Hasse(self$concepts, as.matrix(self$I))

      }

    },

    # Plot the formal context table
    plot_context = function() {

      color_function <- colour_ramp(brewer.pal(9, "Greys"))
      heatmap(t(as.matrix(self$I)), Rowv = NA, Colv = NA,
              col = color_function(seq(0, 1, 0.01)),
              scale = "none")

    },

    # Get support of each concept
    get_concept_support = function() {

      my_I <- self$I
      my_I@x <- as.numeric(my_I@x)

      intents <- lapply(self$concepts, function(s) s[[2]])
      intents <- do.call(cbind, args = intents)

      subsets <- .is_subset_sparse(intents, my_I)

      self$concept_support <- rowMeans(subsets)

      return(self$concept_support)

    },

    # Compute support of each implication
    get_implication_support = function() {

      LHS <- self$implications$get_LHS_matrix()
      my_I <- self$I
      # my_I@x <- as.numeric(my_I@x)

      subsets <- .is_subset_sparse(LHS, my_I)

      self$implications_support <- rowMeans(subsets)

      return(self$implications_support)

    }

  ),

  private = list(

    extents = NULL,

    intents = NULL
  )

)
