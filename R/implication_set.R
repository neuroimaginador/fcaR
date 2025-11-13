#' @title
#' R6 Class for Set of implications
#'
#' @description
#' This class implements the structure needed to store implications and the methods associated.
#'
#' @examples
#' # Build a formal context
#' fc_planets <- FormalContext$new(planets)
#'
#' # Find its implication basis
#' fc_planets$find_implications()
#'
#' # Print implications
#' fc_planets$implications
#'
#' # Cardinality and mean size in the ruleset
#' fc_planets$implications$cardinality()
#' sizes <- fc_planets$implications$size()
#' colMeans(sizes)
#'
#' # Simplify the implication set
#' fc_planets$implications$apply_rules("simplification")
#'
#' @references
#'
#' Ganter B, Obiedkov S (2016). Conceptual Exploration. Springer. https://doi.org/10.1007/978-3-662-49291-8
#'
#' Hahsler M, Grun B, Hornik K (2005). “arules - a computational environment for mining association rules and frequent item sets.” _J Stat Softw_, *14*, 1-25.
#'
#' Belohlavek R, Cordero P, Enciso M, Mora Á, Vychodil V (2016). “Automated prover for attribute dependencies in data with grades.” _International Journal of Approximate Reasoning_, *70*, 51-67.
#'
#' Mora A, Cordero P, Enciso M, Fortes I, Aguilera G (2012). “Closure via functional dependence simplification.” _International Journal of Computer Mathematics_, *89*(4), 510-526.
#'
#' @export
ImplicationSet <- R6::R6Class(
  classname = "ImplicationSet",
  public = list(
    #' @description
    #' Initialize with an optional name
    #'
    #' @param ... See Details.
    #'
    #' @details
    #' Creates and initialize a new \code{ImplicationSet} object. It can be done in two ways:
    #' \code{initialize(name, attributes, lhs, rhs)}
    #' or \code{initialize(rules)}
    #'
    #' In the first way, the only mandatory argument is \code{attributes}, (character vector) which is a vector of names of the attributes on which we define the implications. Optional arguments are: \code{name} (character string), name of the implication set, \code{lhs} (a \code{dgCMatrix}), initial LHS of the implications stored and the analogous \code{rhs}.
    #'
    #' The other way is used to initialize the \code{ImplicationSet} object from a \code{rules} object from package \code{arules}.
    #'
    #' @return A new \code{ImplicationSet} object.
    initialize = function(...) {
      dots <- list(...)

      classes <- lapply(dots, function(d) class(d)[1])

      any_rules <- which(classes == "rules")

      if (length(any_rules) > 0) {
        arules_imp <- dots[[any_rules[1]]]

        attributes <- arules_imp@lhs@itemInfo$labels
        private$attributes <- attributes

        name <- as.character(arules_imp@info$data)
        private$name <- name

        private$lhs_matrix <- convert_to_sparse(arules_imp@lhs@data)
        private$rhs_matrix <- convert_to_sparse(arules_imp@rhs@data)

        rownames(private$lhs_matrix) <- private$attributes
        rownames(private$rhs_matrix) <- private$attributes
      } else {
        args <- list(
          name = "",
          attributes = c(),
          lhs = NULL,
          rhs = NULL,
          I = NULL
        )
        args[names(dots)] <- dots
        private$name <- args$name
        private$attributes <- args$attributes
        private$lhs_matrix <- args$lhs
        private$rhs_matrix <- args$rhs
        private$I <- args$I
        private$implication_support <- numeric(0)
      }
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
    #' Get a subset of the implication set
    #'
    #' @param idx   (integer or logical vector) Indices of the implications to extract or remove. If logical vector, only \code{TRUE} elements are retained and the rest discarded.
    #'
    #' @return A new \code{ImplicationSet} with only the rules given by the \code{idx} indices (if all \code{idx > 0} and all but \code{idx} if all \code{idx < 0}.
    #' @export
    `[` = function(idx) {
      if (is.logical(idx)) {
        idx <- which(idx)
      }

      idx <- idx[abs(idx) <= self$cardinality()]
      idx <- idx[abs(idx) > 0]

      if (length(idx) > 0) {
        if (all(idx < 0) | all(idx > 0)) {
          imp <- ImplicationSet$new(
            name = paste0(private$name, "_", paste0(idx)),
            attributes = private$attributes,
            lhs = Matrix::Matrix(private$lhs_matrix[, idx],
              sparse = TRUE
            ),
            rhs = Matrix::Matrix(private$rhs_matrix[, idx],
              sparse = TRUE
            )
          )

          return(imp)
        } else {
          stop("Cannot use mixed positive and negative indices.\n",
            call. = FALSE
          )
        }
      } else {
        return(ImplicationSet$new(
          attributes = private$attributes,
          I = private$I
        ))
      }
    },

    #' @description
    #' Convert to arules format
    #'
    #' @param quality   (logical) Compute the interest measures for each rule?
    #'
    #' @return A \code{rules} object as used by package \code{arules}.
    #'
    #' @export
    to_arules = function(quality = TRUE) {
      if (self$is_empty()) {
        stop("No implications to export.\n",
          call. = FALSE
        )
      }

      if (!private$is_binary()) {
        stop("Export to arules format is only allowed when using binary implications.\n",
          call. = FALSE
        )
      }

      rules <- imps_to_arules(
        LHS = private$lhs_matrix,
        RHS = private$rhs_matrix,
        attributes = private$attributes,
        I = private$I,
        quality = quality
      )


      return(rules)
    },

    #' @description
    #' Add a precomputed implication set
    #'
    #' @param ...   An \code{ImplicationSet} object, a \code{rules} object, or a pair \code{lhs}, \code{rhs} of \code{Set} objects or \code{dgCMatrix}. The implications to add to this formal context.
    #'
    #' @return Nothing, just updates the internal \code{implications} field.
    #'
    #' @export
    add = function(...) {
      dots <- list(...)

      # Just a single ImplicationSet
      if (length(dots) == 1) {
        if (inherits(dots[[1]], "ImplicationSet")) {
          private$append_implications(dots[[1]])
        } else {
          # It is a rules object
          implications <- ImplicationSet$new(dots[[1]])
          private$append_implications(implications)
        }
      } else {
        # It must come in the form LHS, RHS
        if (length(dots) == 2) {
          lhs <- dots[[1]]
          rhs <- dots[[2]]

          if (inherits(lhs, "Set")) {
            lhs <- lhs$get_vector()
          }

          if (inherits(rhs, "Set")) {
            rhs <- rhs$get_vector()
          }

          imp <- ImplicationSet$new(
            attributes = private$attributes,
            lhs = lhs,
            rhs = rhs
          )

          private$append_implications(imp)
        } else {
          stop("Invalid number of arguments.\n",
            call. = FALSE
          )
        }
      }
    },

    #' @description
    #' Cardinality: Number of implications in the set
    #'
    #' @return The cardinality of the implication set.
    #'
    #' @export
    cardinality = function() {
      if (self$is_empty()) {
        return(0)
      }

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
      lhs_size <- Matrix::colSums(private$lhs_matrix)
      rhs_size <- Matrix::colSums(private$rhs_matrix)

      return(cbind(LHS = lhs_size, RHS = rhs_size))
    },

    #' @description
    #' Compute the semantic closure of a fuzzy set with respect to the implication set
    #'
    #' @param S        (a \code{Set} object)  Fuzzy set to compute its closure. Use class \code{Set} to build it.
    #' @param reduce   (logical) Reduce the implications using simplification logic?
    #' @param verbose  (logical) Show verbose output?
    #'
    #' @return If \code{reduce == FALSE}, the output is a fuzzy set corresponding to the closure of \code{S}. If \code{reduce == TRUE}, a list with two components: \code{closure}, with the closure as above, and \code{implications}, the reduced set of implications.
    #'
    #' @export
    closure = function(S,
                       reduce = FALSE,
                       verbose = FALSE) {
      if (inherits(S, "Set")) {
        original <- S$clone()
        S <- match_attributes(S, private$attributes)
        S <- S$get_vector()
      } else {
        original <- Set$new(private$attributes, M = S)
      }

      if (private$hedge == "identity") {
        cl <- .compute_closure_fuzzy(
          S,
          LHS = private$lhs_matrix,
          RHS = private$rhs_matrix,
          attributes = private$attributes,
          reduce = reduce,
          verbose = verbose,
          is_direct = private$directness,
          logic = private$logic
        )
      } else { # Should be globalization
        cl <- .compute_closure(
          S,
          LHS = private$lhs_matrix,
          RHS = private$rhs_matrix,
          attributes = private$attributes,
          reduce = reduce,
          verbose = verbose,
          is_direct = private$directness
        )
      }

      if (!reduce) {
        cl <- list(closure = Set$new(
          attributes = private$attributes,
          M = cl$closure
        ))
      } else {
        cl$closure <- Set$new(
          attributes = private$attributes,
          M = cl$closure
        )

        cl$implications <- ImplicationSet$new(
          attributes = private$attributes,
          name = "reduced",
          lhs = cl$implications$lhs,
          rhs = cl$implications$rhs,
          I = private$I
        )
      }

      cl$closure <- match_attributes(
        cl$closure,
        original$get_attributes()
      )

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
      if (inherits(S, "Set")) {
        S <- S$get_vector()
      }

      .recommend_attribute(
        S = S,
        LHS = private$lhs_matrix,
        RHS = private$rhs_matrix,
        attribute_filter = attribute_filter,
        attributes = private$attributes
      )
    },

    #' @description
    #' Apply rules to remove redundancies
    #'
    #' @param rules       (character vector) Names of the rules to use. See \code{details}.
    #' @param batch_size  (integer) If the number of rules is large, apply the rules by batches of this size.
    #' @param parallelize (logical) If possible, should we parallelize the computation among different batches?
    #' @param reorder     (logical) Should the rules be randomly reordered previous to the computation?
    #'
    #' @details
    #' Currently, the implemented rules are \code{"generalization"}, \code{"simplification"}, \code{"reduction"} and \code{"composition"}.
    #'
    #' @return Nothing, just updates the internal matrices for LHS and RHS.
    #'
    #' @export
    apply_rules = function(rules = c("composition", "generalization"),
                           batch_size = 25000L,
                           parallelize = FALSE,
                           reorder = FALSE) {
      # If no implications, do nothing
      if (is.null(private$lhs_matrix) || (ncol(private$lhs_matrix) == 0)) {
        return(invisible(self))
      }

      # If one implication,
      # just "reduction" can be done
      if (ncol(private$lhs_matrix) == 1) {
        rules <- intersect(rules, "reduction")
      }


      if (length(rules) > 0) {
        L <- .batch_apply(
          LHS = private$lhs_matrix,
          RHS = private$rhs_matrix,
          attributes = private$attributes,
          rules = rules,
          parallelize = parallelize,
          batch_size = batch_size,
          reorder = reorder
        )

        private$lhs_matrix <- L$lhs
        private$rhs_matrix <- L$rhs
      }

      return(invisible(self))
    },

    #' @description
    #' Convert Implications to Canonical Basis
    #'
    #' @return The canonical basis of implications obtained from the current \code{ImplicationSet}
    #'
    #' @export
    #'
    to_basis = function() {
      LHS <- private$lhs_matrix
      RHS <- private$rhs_matrix
      attributes <- private$attributes
      L <- .composition(LHS, RHS, attributes)
      LHS <- L$lhs
      RHS <- L$rhs
      L <- .generalization(LHS, RHS, attributes)
      LHS <- L$lhs
      RHS <- L$rhs

      L <- .imp_to_basis(LHS, RHS, attributes)

      private$lhs_matrix <- L$lhs
      private$rhs_matrix <- L$rhs

      return(invisible(self))
    },

    #' @description
    #' Compute the Direct Optimal Basis using optimized C++ algorithms.
    #'
    #' @param method (character) The specific algorithm to run:
    #' \itemize{
    #'   \item \code{"direct_optimal"}: (Default) The Direct Optimal Saturation-Pruning algorithm.
    #'   \item \code{"final_ts"}: Computes Transitive Closure then Prunes (Standard approach).
    #'   \item \code{"monotonic"}: Incremental algorithm maintaining monotonicity.
    #'   \item \code{"priority"}: Priority-based refinement algorithm.
    #' }
    #' @param verbose (logical) Print verbose output from the C++ backend.
    #'
    #' @return Nothing, updates the \code{ImplicationSet} in place with the new basis.
    #' @export
    to_direct_optimal = function(method = c("direct_optimal", "final_ts", "monotonic", "priority"),
                                 verbose = FALSE) {
      method <- match.arg(method)

      # TODO: What happens with different hedges??!!

      # 1. Prepare Inputs for C++
      # Calculate L (set of truth degrees present in data + 0 + 1)
      vals <- unique(c(0, 1, private$lhs_matrix@x, private$rhs_matrix@x))
      vals <- sort(vals)

      # Retrieve the logic name from the private field
      # Note: Ensure private$logic holds one of: "Lukasiewicz", "Zadeh", "Godel", "Product"
      current_logic <- private$logic

      if (is.null(current_logic)) {
        current_logic <- "Lukasiewicz" # Default fallback
      }

      # 2. Call C++
      # Now passing 'current_logic' as an argument
      res_list <- switch(method,
        "direct_optimal" = run_direct_optimal_sp_rcpp_optimized(
          private$lhs_matrix, private$rhs_matrix, private$attributes, vals, current_logic, TRUE, verbose
        ),
        "final_ts" = run_final_ts_rcpp_optimized(
          private$lhs_matrix, private$rhs_matrix, private$attributes, vals, current_logic, TRUE, verbose
        ),
        "monotonic" = run_monotonic_incremental_rcpp_optimized(
          private$lhs_matrix, private$rhs_matrix, private$attributes, vals, current_logic, TRUE, verbose
        ),
        "priority" = run_priority_refinement_rcpp_optimized(
          private$lhs_matrix, private$rhs_matrix, private$attributes, vals, current_logic, TRUE, verbose
        )
      )

      # 3. Reconstruct Sparse Matrices
      s_data <- res_list$Sigma
      n_rules <- s_data$n_rules
      n_attrs <- length(private$attributes)

      if (n_rules > 0) {
        # Convert 0-based indices (C++) to 1-based indices (R)
        new_lhs <- Matrix::sparseMatrix(
          i = s_data$lhs_i + 1,
          j = s_data$lhs_j + 1,
          x = s_data$lhs_x,
          dims = c(n_attrs, n_rules),
          dimnames = list(private$attributes, NULL)
        )

        new_rhs <- Matrix::sparseMatrix(
          i = s_data$rhs_i + 1,
          j = s_data$rhs_j + 1,
          x = s_data$rhs_x,
          dims = c(n_attrs, n_rules),
          dimnames = list(private$attributes, NULL)
        )
      } else {
        new_lhs <- Matrix::Matrix(0, nrow = n_attrs, ncol = 0, sparse = TRUE)
        new_rhs <- Matrix::Matrix(0, nrow = n_attrs, ncol = 0, sparse = TRUE)
      }

      # 4. Update Internal State
      private$lhs_matrix <- new_lhs
      private$rhs_matrix <- new_rhs
      private$implication_support <- numeric(0)
      private$directness <- TRUE

      if (verbose) {
        cat("Algorithm finished using logic:", current_logic, "\n")
        print(res_list$metrics)
      }

      return(invisible(self))
    },

    #' @description
    #' Print all implications to text
    #'
    #' @return A string with all the implications in the set.
    #'
    #' @export
    print = function() {
      if (is.null(private$lhs_matrix)) {
        cat("Implication set with 0 implications.\n")

        return(invisible(self))
      }

      n_implications <- ncol(private$lhs_matrix)
      cat("Implication set with", n_implications, "implications.\n")

      if (n_implications > 0) {
        attributes <- private$attributes
        LHS <- private$lhs_matrix
        RHS <- private$rhs_matrix

        implications <- sapply(
          seq(n_implications),
          function(i) {
            paste0(
              "Rule ", i, ": ",
              # function(i) paste0("Rule: ",
              .implication_to_string(LHS[, i], RHS[, i], attributes)
            )
          }
        )

        implications <- sapply(implications, function(s) stringr::str_wrap(s, width = getOption("width"), exdent = 2))

        cat(implications, sep = "\n")
      }
    },

    #' @description
    #' Export to LaTeX
    #'
    #' @param print (logical) Print to output?
    #' @param ncols  (integer) Number of columns for the output.
    #' @param numbered (logical) If \code{TRUE} (default), implications will be numbered in the output.
    #' @param numbers (vector) If \code{numbered}, use these elements to enumerate the implications. The default is to enumerate 1, 2, ..., but can be changed.
    #'
    #' @return A string in LaTeX format that prints nicely all the implications.
    #'
    #' @export
    to_latex = function(print = TRUE,
                        ncols = 1,
                        numbered = TRUE,
                        numbers = seq(self$cardinality())) {
      output <- imp_to_latex(self,
        ncols = ncols,
        numbered = numbered,
        numbers = numbers
      )

      if (print) {
        cat(output)
      }

      return(invisible(output))
    },

    #' @description
    #' Get internal LHS matrix
    #'
    #' @return A sparse matrix representing the LHS of the implications in the set.
    #'
    #' @export
    get_LHS_matrix = function() {
      if (self$is_empty()) {
        LHS <- Matrix::Matrix(FALSE,
          nrow = length(private$attributes),
          ncol = 1,
          sparse = TRUE
        )
      } else {
        LHS <- private$lhs_matrix
      }

      dimnames(LHS) <- list(
        private$attributes,
        paste0(seq(ncol(LHS)))
      )

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
        RHS <- Matrix::Matrix(FALSE,
          nrow = length(private$attributes),
          ncol = 1,
          sparse = TRUE
        )
      } else {
        RHS <- private$rhs_matrix
      }

      dimnames(RHS) <- list(
        private$attributes,
        paste0(seq(ncol(RHS)))
      )


      return(RHS)
    },


    #' @description
    #' Filter implications by attributes in LHS and RHS
    #'
    #' @param lhs  (character vector) Names of the attributes to filter the LHS by. If \code{NULL}, no filtering is done on the LHS.
    #' @param not_lhs  (character vector) Names of the attributes to not include in the LHS. If \code{NULL} (the default), it is not considered at all.
    #' @param rhs  (character vector) Names of the attributes to filter the RHS by. If \code{NULL}, no filtering is done on the RHS.
    #' @param not_rhs  (character vector) Names of the attributes to not include in the RHS. If \code{NULL} (the default), it is not considered at all.
    #' @param drop  (logical) Remove the rest of attributes in RHS?
    #'
    #' @return An \code{ImplicationSet} that is a subset of the current set, only with those rules which has the attributes in \code{lhs} and \code{rhs} in their LHS and RHS, respectively.
    #'
    #' @export
    filter = function(lhs = NULL,
                      not_lhs = NULL,
                      rhs = NULL,
                      not_rhs = NULL,
                      drop = FALSE) {
      RHS <- private$rhs_matrix
      LHS <- private$lhs_matrix

      if (!is.null(lhs)) {
        # Filter the implications which have
        # the given lhs
        idx_attr <- match(
          lhs,
          private$attributes
        )

        if (length(idx_attr) > 1) {
          idx_lhs <- Matrix::which(Matrix::colSums(LHS[idx_attr, ]) > 0)
        } else {
          idx_lhs <- which(LHS[idx_attr, ] > 0)
        }
      } else {
        # If not specified a filter for LHS,
        # select all implications
        idx_lhs <- seq(ncol(LHS))
      }

      if (!is.null(not_lhs)) {
        # Filter the implications which
        # does not have the given not_lhs
        idx_attr <- match(
          not_lhs,
          private$attributes
        )

        if (length(idx_attr) > 1) {
          idx_not_lhs <- Matrix::which(Matrix::colSums(LHS[idx_attr, ]) > 0)
        } else {
          idx_not_lhs <- which(LHS[idx_attr, ] > 0)
        }
      } else {
        # If not specified a filter for LHS,
        # select none.
        idx_not_lhs <- c()
      }

      idx_lhs <- setdiff(idx_lhs, idx_not_lhs)

      if (!is.null(rhs)) {
        # Filter the implications which have
        # the given lhs
        idx_attr <- match(
          rhs,
          private$attributes
        )

        if (length(idx_attr) > 1) {
          idx_rhs <- Matrix::which(Matrix::colSums(RHS[idx_attr, ]) > 0)
        } else {
          idx_rhs <- which(RHS[idx_attr, ] > 0)
        }
      } else {
        # If not specified a filter for RHS,
        # select all implications
        idx_rhs <- seq(ncol(RHS))
      }

      if (!is.null(not_rhs)) {
        # Filter the implications which
        # does not have the given not_lhs
        idx_attr <- match(
          not_rhs,
          private$attributes
        )

        if (length(idx_attr) > 1) {
          idx_not_rhs <- Matrix::which(Matrix::colSums(RHS[idx_attr, ]) > 0)
        } else {
          idx_not_rhs <- which(RHS[idx_attr, ] > 0)
        }
      } else {
        # If not specified a filter for LHS,
        # select none.
        idx_not_rhs <- c()
      }

      idx_rhs <- setdiff(idx_rhs, idx_not_rhs)

      idx <- intersect(idx_lhs, idx_rhs)

      if (length(idx) == 0) {
        warning("No combination of given LHS and RHS found.\n",
          call. = FALSE,
          immediate. = TRUE
        )
        return(invisible(NULL))
      }

      if (length(idx) > 0) {
        if (drop && !is.null(rhs)) {
          newLHS <- LHS[, idx]
          newRHS <- RHS[, idx]

          other_idx <- setdiff(seq(nrow(RHS)), idx_attr)
          newRHS[other_idx, ] <- 0

          imp <- ImplicationSet$new(
            name = paste0(private$name, "_filtered"),
            attributes = private$attributes,
            lhs = Matrix::Matrix(newLHS, sparse = TRUE),
            rhs = Matrix::Matrix(newRHS, sparse = TRUE)
          )
        } else {
          imp <- ImplicationSet$new(
            name = paste0(private$name, "_filtered"),
            attributes = private$attributes,
            lhs = Matrix::Matrix(LHS[, idx], sparse = TRUE),
            rhs = Matrix::Matrix(RHS[, idx], sparse = TRUE)
          )
        }

        return(imp)
      }
    },

    #' @description
    #' Compute support of each implication
    #'
    #' @return A vector with the support of each implication
    #' @export
    support = function() {
      if (self$is_empty()) {
        return(numeric(0))
      }

      if (length(private$implication_support) > 0) {
        return(private$implication_support)
      }

      subsets <- .subset(
        private$lhs_matrix,
        private$I
      )

      private$implication_support <- Matrix::rowMeans(subsets)

      return(private$implication_support)
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
    #' Sets the hedge to use when computing closures
    #'
    #'
    #' @param name The name of the hedge to use. Only "globalization" and "identity" are allowed.
    #'
    #' @export
    use_hedge = function(name = c("globalization", "identity")) {
      name <- match.arg(name)
      if (name %in% c("globalization", "identity")) {
        private$hedge <- name
      } else {
        warning("No hedge with that name found.")
      }
    },

    #' @description
    #' Gets the hedge used to compute closures
    #'
    #' @return A string with the name of the hedge
    #'
    #' @export
    get_hedge = function() {
      private$hedge
    }
  ),
  private = list(
    name = "",
    attributes = NULL,
    lhs_matrix = NULL,
    rhs_matrix = NULL,
    I = NULL,
    implication_support = NULL,
    binary = NULL,
    directness = FALSE,
    logic = "Godel",
    hedge = "globalization",
    is_binary = function() {
      if (!is.null(private$binary)) {
        return(private$binary)
      }

      if (!is.null(private$I)) {
        v <- unique(c(0, private$I@x, 1))
      } else {
        if (!is.null(private$lhs_matrix)) {
          v <- unique(c(
            0, private$lhs_matrix@x,
            private$rhs_matrix@x, 1
          ))
        } else {
          return(FALSE)
        }
      }

      private$binary <- (length(v) == 2) && all(v == c(0, 1))
      return(private$binary)
    },
    append_implications = function(implications) {
      imps <- match_implications(
        implications,
        private$attributes
      )

      LHS <- imps$get_LHS_matrix()
      RHS <- imps$get_RHS_matrix()

      if (length(private$attributes) == nrow(LHS)) {
        private$lhs_matrix <- cbind(private$lhs_matrix, LHS)
        private$rhs_matrix <- cbind(private$rhs_matrix, RHS)
      } else {
        stop("Dimensions mismatch.")
      }
    }
  )
)
