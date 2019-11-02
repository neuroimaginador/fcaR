#' @title
#' R6 Class for Set of implications
#'
#' @description
#' This class implements the structure needed to store implications and the methods associated.
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
    initialize = function(name = "",
                          attributes = c(),
                          lhs = NULL,
                          rhs = NULL) {

      private$name <- name
      private$attributes <- attributes
      private$lhs_matrix <- lhs
      private$rhs_matrix <- rhs

    },

    #' @description
    #' Get the names of the attributes
    #'
    #' @return A character vector with the names of the attributes used in the implications.
    #'
    #' @export
    get_attributes = function() {

      return(private$attributes)

    },

    #' @description
    #' Import from arules object
    #'
    #' @param arules_imp (\code{rules} object) The implications obtained with \code{arules} to be imported.
    #'
    #' @return Nothing, just updates the internal matrices for LHS and RHS.
    #'
    #' @import arules
    #' @importFrom methods as is
    #' @export
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

    #' @description
    #' Convert to arules format
    #'
    #' @return A \code{rules} object as used by package \code{arules}.
    #'
    #' @import arules
    #' @importFrom methods as is
    #' @export
    to_arules = function() {

      # Needed to export to arules
      L <- .reduction(LHS = private$lhs_matrix,
                      RHS = private$rhs_matrix,
                      attributes = private$attributes)

      LHS <- as(L$lhs, "ngCMatrix")
      LHS <- as(LHS, "itemMatrix")
      itemLabels(LHS) <- private$attributes

      RHS <- as(L$rhs, "ngCMatrix")
      RHS <- as(RHS, "itemMatrix")
      itemLabels(RHS) <- private$attributes

      rules <- new("rules", lhs = LHS, rhs = RHS)

      return(rules)

    },

    #' @description
    #' Add new implication to the set
    #'
    #' @param lhs  (a sparse matrix) The LHS of the rule to add.
    #' @param rhs  (a sparse matrix) The RHS of the rule to add.
    #'
    #' @return Nothing, just updates the internal matrices for LHS and RHS.
    #'
    #' @export
    add_implication = function(lhs, rhs) {

      if (inherits(lhs, "SparseSet")) {

        lhs <- lhs$get_vector()

      }

      if (inherits(rhs, "SparseSet")) {

        rhs <- rhs$get_vector()

      }

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

    },

    #' @description
    #' Append implications to the current ones.
    #'
    #' @param implications (\code{ImplicationSet}) The set of implications to append at the end of the current set.
    #'
    #' @return Nothing, just updates the internal matrices for LHS and RHS.
    #'
    #' @export
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

    #' @description
    #' Cardinality: Number of implications in the set
    #'
    #' @return The cardinality of the implication set.
    #'
    #' @export
    cardinality = function() {

      if (self$is_empty()) return(0)

      ncol(private$lhs_matrix)

    },

    #' @description
    #' Empty set
    #'
    #' @return \code{TRUE} if the set of implications is empty, \code{FALSE} otherwise.
    #'
    #' @export
    is_empty = function() {

      is.null(private$lhs_matrix)

    },

    #' @description
    #' Size: number of attributes in each of LHS and RHS
    #'
    #' @return A vector with two components: the number of attributes present in each of the LHS and RHS of each implication in the set.
    #'
    #' @export
    size = function() {

      lhs_size <- colSums(private$lhs_matrix)
      rhs_size <- colSums(private$rhs_matrix)

      return(cbind(LHS = lhs_size, RHS = rhs_size))

    },

    #' @description
    #' Compute the semantic closure of a fuzzy set with respect to the implication set
    #'
    #' @param S        (a \code{SparseSet} object)  Fuzzy set to compute its closure. Use class \code{SparseSet} to build it.
    #' @param reduce   (logical) Reduce the implications using simplification logic?
    #' @param verbose  (logical) Show verbose output?
    #'
    #' @return If \code{reduce == FALSE}, the output is a fuzzy set corresponding to the closure of \code{S}. If \code{reduce == TRUE}, a list with two components: \code{closure}, with the closure as above, and \code{implications}, the reduced set of implications.
    #'
    #' @export
    compute_closure = function(S,
                               reduce = FALSE,
                               verbose = FALSE) {

      if (inherits(S, "SparseSet")) {

        S <- S$get_vector()

      }

      cl <- .compute_closure(S,
                             LHS = private$lhs_matrix,
                             RHS = private$rhs_matrix,
                             attributes = private$attributes,
                             reduce = reduce,
                             verbose = verbose)

      if (!reduce) {

        cl <- sparse_set$new(attributes = private$attributes,
                             M = cl)

      } else {

        cl$closure <- sparse_set$new(attributes = private$attributes,
                                     M = cl$closure)

        cl$implications <- implication_set$new(attributes = private$attributes,
                                               name = "reduced",
                                               lhs = cl$implications$lhs,
                                               rhs = cl$implications$rhs)

      }

      return(cl)

    },

    #' @description
    #' Generate a recommendation for a subset of the attributes
    #'
    #' @param S        (a vector) Vector with the grades of each attribute (a fuzzy set).
    #' @param attribute_filter (character vector) Names of the attributes to get recommendation for.
    #'
    #' @return A fuzzy set describing the values of the attributes in \code{attribute_filter} within the closure of \code{S}.
    #'
    #' @export
    recommend = function(S, attribute_filter) {

      if (inherits(S, "SparseSet")) {

        S <- S$get_vector()

      }

      .recommend_attribute(S = S,
                           LHS = private$lhs_matrix,
                           RHS = private$rhs_matrix,
                           attribute_filter = attribute_filter,
                           attributes = private$attributes)

    },

    #' @description
    #' Apply rules to remove redundancies
    #'
    #' @param rules       (character vector) Names of the rules to use. See \code{details}.
    #' @param batch_size  (integer) If the number of rules is large, apply the rules by batches of this size.
    #' @param parallelize (logical) If possible, should we parallelize the computation among different batches?
    #' @param reorder     (logical) Should the rules be randonly reordered previous to the computation?
    #'
    #' @details
    #' Currently, the implemented rules are \code{"generalization"}, \code{"simplification"}, \code{"reduction"} and \code{"composition"}.
    #'
    #' @return Nothing, just updates the internal matrices for LHS and RHS.
    #'
    #' @export
    apply_rules = function(rules = c("composition", "generalization"),
                           batch_size = 25000L,
                           parallelize = TRUE,
                           reorder= FALSE) {

      L <- .batch_apply(LHS = private$lhs_matrix,
                        RHS = private$rhs_matrix,
                        attributes = private$attributes,
                        rules = rules,
                        parallelize = parallelize,
                        batch_size = batch_size,
                        reorder = reorder)

      private$lhs_matrix <- L$lhs
      private$rhs_matrix <- L$rhs

    },

    #' @description
    #' Print all implications to text
    #'
    #' @return A string with all the implications in the set.
    #'
    #' @export
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

    #' @description
    #' Export to LaTeX
    #'
    #' @param ncols  (integer) Number of columns for the output.
    #'
    #' @return A string in LaTeX format that prints nicely all the implications.
    #'
    #' @export
    to_latex = function(ncols = 1) {

      imp_to_latex(self, ncols = ncols)


    },

    #' @description
    #' Get internal LHS matrix
    #'
    #' @return A sparse matrix representing the LHS of the implications in the set.
    #'
    #' @export
    get_LHS_matrix = function() {

      if (self$is_empty()) {

        LHS <- Matrix(FALSE,
                      nrow = length(private$attributes),
                      ncol = 1,
                      sparse = TRUE)

      } else {

        LHS <- private$lhs_matrix

      }

      dimnames(LHS) <- list(private$attributes,
                            paste0(seq(ncol(LHS))))

      return(LHS)

    },

    #' @description
    #' Get internal RHS matrix
    #'
    #' @return A sparse matrix representing the RHS of the implications in the set.
    #'
    #' @export
    get_RHS_matrix = function() {

      if (self$is_empty()) {

        RHS <- Matrix(FALSE,
                      nrow = length(private$attributes),
                      ncol = 1,
                      sparse = TRUE)

      } else {

        RHS <- private$rhs_matrix

      }

      dimnames(RHS) <- list(private$attributes,
                            paste0(seq(ncol(RHS))))


      return(RHS)

    },

    #' @description
    #' Filter implications by attributes in LHS and RHS
    #'
    #' @param lhs  (character vector) Names of the attributes to filter the LHS by. If \code{NULL}, no filtering is done on the LHS.
    #' @param rhs  (character vector) Names of the attributes to filter the RHS by. If \code{NULL}, no filtering is done on the RHS.
    #' @param drop  (logical) Remove the rest of attributes in RHS?
    #'
    #' @return An \code{ImplicationSet} that is a subset of the current set, only with those rules which has the attributes in \code{lhs} and \code{rhs} in their LHS and RHS, respectively.
    #'
    #' @export
    filter = function(lhs = NULL,
                      rhs = NULL,
                      drop = FALSE) {

      RHS <- private$rhs_matrix
      LHS <- private$lhs_matrix

      if (!is.null(lhs)) {

        # Filter the implications which have
        # the given lhs
        idx_attr <- match(lhs,
                          private$attributes)

        if (length(idx_attr) > 1) {

          idx_lhs <- which(colSums(LHS[idx_attr, ]) > 0)

        } else {

          idx_lhs <- which(LHS[idx_attr, ] > 0)

        }

      } else {

        # If not specified a filter for LHS,
        # select all implications
        idx_lhs <- seq(ncol(LHS))

      }

      if (!is.null(rhs)) {

        # Filter the implications which have
        # the given lhs
        idx_attr <- match(rhs,
                          private$attributes)

        if (length(idx_attr) > 1) {

          idx_rhs <- which(colSums(RHS[idx_attr, ]) > 0)

        } else {

          idx_rhs <- which(RHS[idx_attr, ] > 0)

        }

      } else {

        # If not specified a filter for RHS,
        # select all implications
        idx_rhs <- seq(ncol(RHS))

      }

      idx <- intersect(idx_lhs, idx_rhs)

      if (length(idx) == 0) {

        warning("No combination of given LHS and RHS found.\n",
                call. = FALSE,
                immediate. = TRUE)
        return(invisible(NULL))

      }

      if (length(idx) > 0) {

        if (drop && !is.null(rhs)) {

          newLHS <- LHS[, idx]
          newRHS <- RHS[, idx]

          other_idx <- setdiff(seq(nrow(RHS)), idx_attr)
          newRHS[other_idx, ] <- 0

          imp <- implication_set$new(name = paste0(private$name, "_filtered"),
                                     attributes = private$attributes,
                                     lhs = Matrix(newLHS, sparse = TRUE),
                                     rhs = Matrix(newRHS, sparse = TRUE))

        } else {

          imp <- implication_set$new(name = paste0(private$name, "_filtered"),
                                     attributes = private$attributes,
                                     lhs = Matrix(LHS[, idx], sparse = TRUE),
                                     rhs = Matrix(RHS[, idx], sparse = TRUE))

        }

        return(imp)

      }

    },

    #' @description
    #' Get a subset of the implication set
    #'
    #' @param idx   (integer vector) Indices of the implications to extract.
    #'
    #' @return A new \code{ImplicationSet} with only the rules given by the \code{idx} indices.
    #'
    #' @export
    get_rules = function(idx) {

      RHS <- private$rhs_matrix
      LHS <- private$lhs_matrix

      imp <- implication_set$new(name = paste0(private$name, "_", paste0(idx)),
                                 attributes = private$attributes,
                                 lhs = Matrix(LHS[, idx], sparse = TRUE),
                                 rhs = Matrix(RHS[, idx], sparse = TRUE))

      return(imp)

    },

    #' @description
    #' Remove implications from the set
    #'
    #' @param idx  (integer vector) Indices of implications to remove.
    #'
    #' @return An \code{ImplicationSet} with the implications at given indices removed with respect to the original set.
    #'
    #' @export
    remove_rules = function(idx) {

      idx <- idx[idx <= ncol(private$lhs_matrix)]

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
