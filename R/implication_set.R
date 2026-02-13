#' @title
#' R6 class for an Implication Set
#'
#' @description
#' This class implements an implication set (LHS -> RHS) in the framework of
#' Formal Concept Analysis (FCA). It inherits from \code{RuleSet} and adds
#' FCA-specific methods such as closure computation, simplification, and
#' basis transformation.
#'
#' @importFrom Matrix colSums Matrix
#' @importFrom stringr str_wrap
#' @importFrom methods new
#'
#' @export
ImplicationSet <- R6::R6Class(
  "ImplicationSet",
  inherit = RuleSet,

  public = list(
    #' @description
    #' Initialize an ImplicationSet
    #'
    #' @param ...   A \code{rules} object (from \code{arules}) or named arguments:
    #' \code{name} (string), \code{attributes} (character vector),
    #' \code{lhs} and \code{rhs} (sparse matrices), \code{I} (incidence matrix).
    #'
    #' @return A new \code{ImplicationSet} object.
    #'
    #' @export
    initialize = function(...) {
      dots <- list(...)

      classes <- lapply(dots, function(d) class(d)[1])

      any_rules <- which(classes == "rules")

      if (length(any_rules) > 0) {
        check_needed_pkg("arules", "initialize from arules objects")
        arules_imp <- dots[[any_rules[1]]]

        attributes <- arules_imp@lhs@itemInfo$labels
        private$attributes <- attributes

        name <- as.character(arules_imp@info$data)
        private$name <- name

        private$lhs_matrix <- convert_to_sparse(arules_imp@lhs@data)
        private$rhs_matrix <- convert_to_sparse(arules_imp@rhs@data)

        rownames(private$lhs_matrix) <- private$attributes
        rownames(private$rhs_matrix) <- private$attributes

        private$quality <- data.frame()
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
        private$quality <- data.frame()
        private$implication_support <- numeric(0)
      }
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
      check_needed_pkg("arules", "import from arules objects")
      dots <- list(...)

      # Just a single ImplicationSet
      if (length(dots) == 1) {
        if (inherits(dots[[1]], "ImplicationSet")) {
          private$append_rules(dots[[1]])
        } else {
          # It is a rules object
          implications <- ImplicationSet$new(dots[[1]])
          private$append_rules(implications)
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

          private$append_rules(imp)
        } else {
          stop("Invalid number of arguments.\n", call. = FALSE)
        }
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
      check_needed_pkg("arules", "export to arules format")
      if (self$is_empty()) {
        stop("No implications to export.\n", call. = FALSE)
      }

      if (!private$is_binary()) {
        stop(
          "Export to arules format is only allowed when using binary implications.\n",
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
              "Rule ",
              i,
              ": ",
              .implication_to_string(LHS[, i], RHS[, i], attributes)
            )
          }
        )

        implications <- sapply(implications, function(s) {
          stringr::str_wrap(s, width = getOption("width"), exdent = 2)
        })

        cat(implications, sep = "\n")
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

      if (is.null(private$I)) {
        return(numeric(0))
      }

      subsets <- .subset(
        private$lhs_matrix,
        private$I
      )

      private$implication_support <- Matrix::rowMeans(subsets)

      return(private$implication_support)
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
    closure = function(S, reduce = FALSE, verbose = FALSE) {
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
      } else {
        # Should be globalization
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
        cl <- list(
          closure = Set$new(
            attributes = private$attributes,
            M = cl$closure
          )
        )
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
    apply_rules = function(
      rules = c("composition", "generalization"),
      batch_size = 25000L,
      parallelize = FALSE,
      reorder = FALSE
    ) {
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
        L <- .process_batch(
          LHS = private$lhs_matrix,
          RHS = private$rhs_matrix,
          attributes = private$attributes,
          rules = rules,
          verbose = FALSE
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
    to_direct_optimal = function(
      method = c(
        "do_sp",
        "direct_optimal",
        "final_ts",
        "monotonic",
        "priority"
      ),
      verbose = FALSE
    ) {
      method <- match.arg(method)

      # 1. Prepare Inputs for C++
      vals <- unique(c(0, 1, private$lhs_matrix@x, private$rhs_matrix@x))
      vals <- sort(vals)

      current_logic <- private$logic

      if (is.null(current_logic)) {
        current_logic <- "Lukasiewicz" # Default fallback
      }

      # 2. Call C++
      res_list <- switch(
        method,
        "do_sp" = run_direct_optimal_sp_single_pass_rcpp_optimized(
          private$lhs_matrix,
          private$rhs_matrix,
          private$attributes,
          vals,
          current_logic,
          TRUE,
          verbose
        ),
        "direct_optimal" = run_direct_optimal_sp_rcpp_optimized(
          private$lhs_matrix,
          private$rhs_matrix,
          private$attributes,
          vals,
          current_logic,
          TRUE,
          verbose
        ),
        "final_ts" = run_final_ts_rcpp_optimized(
          private$lhs_matrix,
          private$rhs_matrix,
          private$attributes,
          vals,
          current_logic,
          TRUE,
          verbose
        ),
        "monotonic" = run_monotonic_incremental_rcpp_optimized(
          private$lhs_matrix,
          private$rhs_matrix,
          private$attributes,
          vals,
          current_logic,
          TRUE,
          verbose
        ),
        "priority" = run_priority_refinement_rcpp_optimized(
          private$lhs_matrix,
          private$rhs_matrix,
          private$attributes,
          vals,
          current_logic,
          TRUE,
          verbose
        )
      )

      # 3. Reconstruct Sparse Matrices
      s_data <- res_list$Sigma
      n_rules <- s_data$n_rules
      n_attrs <- length(private$attributes)

      if (n_rules > 0) {
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

      cat("Algorithm finished using logic:", current_logic, "\n")
      print(res_list$metrics)

      return(invisible(self))
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
    },

    #' @description
    #' Export the implication set to JSON
    #'
    #' @param file        (character) The path of the file to save the JSON to.
    #' @param return_list (logical) If TRUE, returns the list representation instead of the JSON string.
    #'
    #' @return A JSON string representing the implication set, or a list if \code{return_list} is TRUE.
    #' @export
    to_json = function(file = NULL, return_list = FALSE) {
      check_needed_pkg("jsonlite", "export to JSON")

      if (self$is_empty()) {
        out <- list(
          type = private$json_type_name(),
          attributes = private$attributes,
          rules = list()
        )
        if (return_list) {
          return(out)
        }
        return(jsonlite::toJSON(out, auto_unbox = TRUE))
      }

      # Determine if fuzzy or binary
      is_bin <- private$is_binary()

      mat_to_list <- function(M, attrs) {
        if (is.null(M)) {
          return(replicate(length(attrs), list()))
        }

        # Force conversion to general triplet
        T <- as(M, "TsparseMatrix")

        if (length(T@i) == 0) {
          return(replicate(ncol(M), list()))
        }

        # Build data frame with 1-based indices
        df_all <- data.frame(
          attr = attrs[T@i + 1],
          rule = T@j + 1
        )

        res <- split(
          df_all$attr,
          factor(df_all$rule, levels = seq_len(ncol(M)))
        )
        return(res)
      }

      lhs_list <- mat_to_list(private$lhs_matrix, private$attributes)
      rhs_list <- mat_to_list(private$rhs_matrix, private$attributes)

      n_imps <- self$cardinality()

      rules <- lapply(seq_len(n_imps), function(i) {
        r <- list(
          lhs = lhs_list[[i]],
          rhs = rhs_list[[i]]
        )
        return(r)
      })

      if (
        !is.null(private$implication_support) &&
          length(private$implication_support) > 0
      ) {
        for (i in seq_along(rules)) {
          rules[[i]]$support <- private$implication_support[i]
        }
      }

      out <- list(
        type = private$json_type_name(),
        attributes = private$attributes,
        rules = rules
      )

      if (return_list) {
        return(out)
      }

      json <- jsonlite::toJSON(out, auto_unbox = TRUE)

      if (!is.null(file)) {
        writeLines(json, file)
        return(invisible(json))
      } else {
        return(json)
      }
    }
  ),

  private = list(
    # ImplicationSet-specific fields
    implication_support = NULL,
    binary = NULL,
    directness = FALSE,
    logic = "Godel",
    hedge = "globalization",

    # Override parent helpers
    rule_type_label = function() "Implication",
    json_type_name = function() "ImplicationSet",

    # Override is_binary with caching version
    is_binary = function() {
      if (!is.null(private$binary)) {
        return(private$binary)
      }

      if (!is.null(private$I)) {
        v <- unique(c(0, private$I@x, 1))
      } else {
        if (!is.null(private$lhs_matrix)) {
          v <- unique(c(
            0,
            private$lhs_matrix@x,
            private$rhs_matrix@x,
            1
          ))
        } else {
          return(FALSE)
        }
      }

      private$binary <- (length(v) == 2) && all(v == c(0, 1))
      return(private$binary)
    },

    # Override factory method
    create_subset = function(...) {
      ImplicationSet$new(...)
    },

    # Override append to match attributes
    append_rules = function(implications) {
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
#' @title Import ImplicationSet from JSON
#' @description Reconstructs an ImplicationSet object from a JSON string.
#' @param json_str A JSON string generated by \code{to_json()}.
#' @return An \code{ImplicationSet} object.
#' @export
implications_from_json <- function(json_str) {
  check_needed_pkg("jsonlite", "import from JSON")

  data <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)

  if (data$type != "ImplicationSet") {
    stop("Invalid JSON: type must be 'ImplicationSet'")
  }

  attributes <- unlist(data$attributes)
  n_rules <- length(data$rules)

  if (n_rules == 0) {
    return(ImplicationSet$new(attributes = attributes))
  }

  # Reconstruct matrices
  # We need to parse LHS and RHS from the rules list

  # Helper to parse list of attributes (binary) or named list (fuzzy)
  parse_part <- function(part_list) {
    # We build a triplet matrix
    i <- integer(0) # Attribute indices
    j <- integer(0) # Rule indices
    x <- numeric(0) # Values

    for (rule_idx in seq_along(part_list)) {
      item <- part_list[[rule_idx]]
      if (length(item) > 0) {
        # Check if it has names (fuzzy) or is just a list of strings (binary)
        if (!is.null(names(item))) {
          # Fuzzy: item is list(attr1 = val1, attr2 = val2)
          attrs <- names(item)
          vals <- unlist(item)
        } else {
          # Binary: item is list("attr1", "attr2") or vector
          attrs <- unlist(item)
          vals <- rep(1, length(attrs))
        }

        # Match attributes to indices
        attr_idxs <- match(attrs, attributes)

        if (any(is.na(attr_idxs))) {
          warning(sprintf(
            "Unknown attributes in rule %d: %s",
            rule_idx,
            paste(attrs[is.na(attr_idxs)], collapse = ", ")
          ))
          # Filter valid ones
          valid <- !is.na(attr_idxs)
          attr_idxs <- attr_idxs[valid]
          vals <- vals[valid]
        }

        if (length(attr_idxs) > 0) {
          i <- c(i, attr_idxs)
          j <- c(j, rep(rule_idx, length(attr_idxs)))
          x <- c(x, vals)
        }
      }
    }

    Matrix::sparseMatrix(
      i = i,
      j = j,
      x = x,
      dims = c(length(attributes), n_rules),
      dimnames = list(attributes, NULL)
    )
  }

  lhs_data <- lapply(data$rules, function(r) r$lhs)
  rhs_data <- lapply(data$rules, function(r) r$rhs)

  LHS <- parse_part(lhs_data)
  RHS <- parse_part(rhs_data)

  IS <- ImplicationSet$new(attributes = attributes, lhs = LHS, rhs = RHS)

  return(IS)
}
