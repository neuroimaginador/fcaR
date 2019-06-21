#' import sets Matrix
implication_set <- R6::R6Class(

  classname = "ImplicationSet",

  public = list(

    # Initialize with an optional name
    initialize = function(name = "",
                          attributes = c(),
                          lhs = NULL,
                          rhs = NULL) {

      private$name <- name
      private$attributes <- attributes
      private$lhs_matrix <- lhs
      private$rhs_matrix <- rhs

    },

    # Import from arules object
    from_arules = function(arules_imp) {

      attributes <- arules_imp@lhs@itemInfo$labels
      private$attributes <- attributes

      name <- as.character(arules_imp@info$data)
      private$name <- name

      private$lhs_matrix <- as(arules_imp@lhs@data, "dgCMatrix")
      private$rhs_matrix <- as(arules_imp@rhs@data, "dgCMatrix")

      rownames(private$lhs_matrix) <- private$attributes
      rownames(private$rhs_matrix) <- private$attributes

    },

    to_arules = function() {

      LHS <- as(private$lhs_matrix, "ngCMatrix")
      LHS <- as(LHS, "itemMatrix")
      itemLabels(LHS) <- private$attributes

      RHS <- as(private$rhs_matrix, "ngCMatrix")
      RHS <- as(RHS, "itemMatrix")
      itemLabels(RHS) <- private$attributes

      rules <- new("rules", lhs = LHS, rhs = RHS)

      return(rules)

    },

    # Number of implications in the set
    length = function() {

      ncol(private$lhs_matrix)

    },

    # Add new implication
    add_implication = function(lhs, rhs) {

      if (is.null(private$lhs_matrix)) {

        private$lhs_matrix <- lhs

      } else {

        private$lhs_matrix <- add_col(private$lhs_matrix, lhs)

      }

      if (is.null(private$rhs_matrix)) {

        private$rhs_matrix <- rhs

      } else {

        private$rhs_matrix <- add_col(private$rhs_matrix, rhs)

      }

      rownames(private$lhs_matrix) <- private$attributes
      rownames(private$rhs_matrix) <- private$attributes

    },

    # Compute the sintactic closure of a set wrt the implications
    compute_closure = function(S, reduce = FALSE) {

      .compute_closure(S,
                       LHS = private$lhs_matrix,
                       RHS = private$rhs_matrix,
                       reduce = reduce)

    },

    recommend = function(S, attribute_filter) {

      .recommend_attribute(S = S,
                           LHS = private$lhs_matrix,
                           RHS = private$rhs_matrix,
                           attribute_filter = attribute_filter)

    },

    apply_rules = function(rules = c("composition", "generalization"),
                           batch_size = 25000L,
                           reorder= TRUE) {

      L <- .batch_apply(LHS = private$lhs_matrix,
                        RHS = private$rhs_matrix,
                        attributes = private$attributes,
                        rules = rules,
                        batch_size = batch_size,
                        reorder = reorder)

      private$lhs_matrix <- L$lhs
      private$rhs_matrix <- L$rhs

    },

    # Print all implications to output
    print = function() {

      n_implications <- ncol(private$lhs_matrix)
      attributes <- private$attributes
      LHS <- private$lhs_matrix
      RHS <- private$rhs_matrix

      implications <- sapply(seq(n_implications),
                             function(i) paste0("Rule ", i, ": ",
                                                .implication_to_string(LHS[, i], RHS[, i], attributes)))

      cat(implications, sep = "\n")

    },

    # Get the sparse matrix for LHS
    get_LHS_matrix = function() {

      private$lhs_matrix

    },

    # Get the sparse matrix for RHS
    get_RHS_matrix = function() {

      private$rhs_matrix

    },

    filter_by_rhs = function(attr_filter) {

      RHS <- private$rhs_matrix
      LHS <- private$lhs_matrix

      idx_attr <- match(attr_filter, private$attributes)

      if (length(idx_attr) > 1) {

        idx <- which(colSums(RHS[idx_attr, ]) > 0)

      } else {

        idx <- which(RHS[idx_attr, ] > 0)

      }

      if (length(idx) > 0) {

        imp <- implication_set$new(name = paste0(private$name, "_filter_", attr_filter),
                                   attributes = private$attributes,
                                   lhs = Matrix(LHS[, idx], sparse = TRUE),
                                   rhs = Matrix(RHS[, idx], sparse = TRUE))

        return(imp)

      }

      warning("No RHS with that attribute, sorry.")
      return(invisible(NULL))


    }

  ),

  private = list(

    name = "",

    attributes = NULL,

    lhs_matrix = NULL,
    rhs_matrix = NULL

  )

)
