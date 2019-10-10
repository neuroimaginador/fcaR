#' R6 Class for Set of implications
#'
#' @import Matrix
#' @export
implication_set <- R6::R6Class(

  classname = "ImplicationSet",

  public = list(

    #' @description
    #' Initialize with an optional name
    #'
    #' @param name (character string) Optional name of the implication set.
    #' @param attributes (character vector) Vector of names of the attributes on which we define the implications.
    #' @param lhs (a \code{dgCMatrix}) Optional initial LHS of the implications stored.
    #' @param rhs (a \code{dgCMatrix}) Optional initial RHS of the implications stored.
    #'
    #' @return A new \code{implication_set} object.
    #'
    initialize = function(name = "",
                          attributes = c(),
                          lhs = NULL,
                          rhs = NULL) {

      private$name <- name
      private$attributes <- attributes
      private$lhs_matrix <- lhs
      private$rhs_matrix <- rhs

    },

    get_attributes = function() {

      return(private$attributes)

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

    append_implications = function(implications) {

      LHS <- implications$get_LHS_matrix()
      RHS <- implications$get_RHS_matrix()

      if (length(private$attributes) == nrow(LHS)) {

        private$lhs_matrix <- cbind(private$lhs_matrix, LHS)
        private$rhs_matrix <- cbind(private$rhs_matrix, RHS)

      } else {

        stop("Dimensions mismatch.")

      }

    },

    # Number of implications in the set
    cardinality = function() {

      if (self$is_empty()) return(0)

      ncol(private$lhs_matrix)

    },

    is_empty = function() {

      is.null(private$lhs_matrix)

    },

    size = function() {

      lhs_size <- colSums(private$lhs_matrix)
      rhs_size <- colSums(private$rhs_matrix)

      return(cbind(LHS = lhs_size, RHS = rhs_size))

    },

    # Compute the sintactic closure of a set wrt the implications
    compute_closure = function(S,
                               reduce = FALSE,
                               verbose = FALSE) {

      S <- Matrix(S, sparse = TRUE)

      cl <- .compute_closure2(S,
                             LHS = private$lhs_matrix,
                             RHS = private$rhs_matrix,
                             attributes = private$attributes,
                             reduce = reduce,
                             verbose = verbose)

      if (!reduce) {

        rownames(cl) <- private$attributes

      } else {

        rownames(cl$closure) <- private$attributes
      }

      return(cl)

    },

    recommend = function(S, attribute_filter) {

      .recommend_attribute(S = S,
                           LHS = private$lhs_matrix,
                           RHS = private$rhs_matrix,
                           attribute_filter = attribute_filter,
                           attributes = private$attributes)

    },

    apply_rules = function(rules = c("composition", "generalization"),
                           batch_size = 25000L,
                           reorder= FALSE) {

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

    to_latex = function(ncols = 1) {

      imp_to_latex(self, ncols = ncols)


    },

    # Get the sparse matrix for LHS
    get_LHS_matrix = function() {

      if (self$is_empty()) {

        LHS <- Matrix(FALSE,
               nrow = length(private$attributes),
               ncol = 1,
               sparse = TRUE)

      } else {

        LHS <- private$lhs_matrix

      }

      return(LHS)

    },

    # Get the sparse matrix for RHS
    get_RHS_matrix = function() {

      if (self$is_empty()) {

        RHS <- Matrix(FALSE,
                      nrow = length(private$attributes),
                      ncol = 1,
                      sparse = TRUE)

      } else {

        RHS <- private$rhs_matrix

      }

      return(RHS)

    },

    filter_by_rhs = function(attr_filter, drop = FALSE) {

      RHS <- private$rhs_matrix
      LHS <- private$lhs_matrix

      idx_attr <- match(attr_filter, private$attributes)

      if (length(idx_attr) > 1) {

        idx <- which(colSums(RHS[idx_attr, ]) > 0)

      } else {

        idx <- which(RHS[idx_attr, ] > 0)

      }

      if (length(idx) > 0) {

        if (drop) {

          newLHS <- LHS[, idx]
          newRHS <- RHS[, idx]

          other_idx <- setdiff(seq(nrow(RHS)), idx_attr)
          newRHS[other_idx, ] <- 0

          imp <- implication_set$new(name = paste0(private$name, "_filter_", attr_filter),
                                     attributes = private$attributes,
                                     lhs = Matrix(newLHS, sparse = TRUE),
                                     rhs = Matrix(newRHS, sparse = TRUE))

        } else {

          imp <- implication_set$new(name = paste0(private$name, "_filter_", attr_filter),
                                     attributes = private$attributes,
                                     lhs = Matrix(LHS[, idx], sparse = TRUE),
                                     rhs = Matrix(RHS[, idx], sparse = TRUE))

        }

        return(imp)

      }

      warning("No RHS with that attribute, sorry.")
      return(invisible(NULL))


    },

    filter_by_lhs = function(attr_filter) {

      RHS <- private$rhs_matrix
      LHS <- private$lhs_matrix

      idx_attr <- match(attr_filter, private$attributes)

      if (length(idx_attr) > 1) {

        idx <- which(colSums(LHS[idx_attr, ]) > 0)

      } else {

        idx <- which(LHS[idx_attr, ] > 0)

      }

      if (length(idx) > 0) {


        imp <- implication_set$new(name = paste0(private$name, "_filter_", attr_filter),
                                   attributes = private$attributes,
                                   lhs = Matrix(LHS[, idx], sparse = TRUE),
                                   rhs = Matrix(RHS[, idx], sparse = TRUE))

        return(imp)

      }

      warning("No LHS with that attribute, sorry.")
      return(invisible(NULL))


    },

    get_rules = function(idx) {

      RHS <- private$rhs_matrix
      LHS <- private$lhs_matrix

      imp <- implication_set$new(name = paste0(private$name, "_", paste0(idx)),
                                 attributes = private$attributes,
                                 lhs = Matrix(LHS[, idx], sparse = TRUE),
                                 rhs = Matrix(RHS[, idx], sparse = TRUE))

      return(imp)

    },

    remove_rules = function(idx) {

      idx <- idx[idx < ncol(private$lhs_matrix)]

      if (length(idx) > 0) {

        private$lhs_matrix <- private$lhs_matrix[, -idx]
        private$rhs_matrix <- private$rhs_matrix[, -idx]
      }

    }

  ),

  private = list(

    name = "",

    attributes = NULL,

    lhs_matrix = NULL,
    rhs_matrix = NULL

  )

)
