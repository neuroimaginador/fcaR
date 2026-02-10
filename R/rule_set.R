#' @title
#' R6 Class for Set of association rules
#'
#' @description
#' This class implements the structure needed to store association rules and the methods associated.
#'
#' @import Matrix
#' @export
RuleSet <- R6::R6Class(
  lock_objects = FALSE,
  parent_env = asNamespace("fcaR"),

  classname = "RuleSet",

  public = list(
    #' @description
    #' Initialize with an optional name
    #'
    #' @param ... See Details.
    #'
    #' @details
    #' Creates and initialize a new \code{RuleSet} object. It can be done in two ways:
    #' \code{initialize(name, attributes, lhs, rhs, quality)}
    #' or \code{initialize(rules)}
    #'
    #' In the first way, the only mandatory argument is \code{attributes}, (character vector) which is a vector of names of the attributes on which we define the rules.
    #'
    #' The other way is used to initialize the \code{RuleSet} object from a \code{rules} object from package \code{arules}.
    #'
    #' @return A new \code{RuleSet} object.
    initialize = function(...) {
      dots <- list(...)

      classes <- lapply(dots, function(d) class(d)[1])

      any_rules <- which(classes == "rules")

      if (length(any_rules) > 0) {
        arules_imp <- dots[[any_rules[1]]]

        attributes <- arules_imp@lhs@itemInfo$labels
        private$attributes <- attributes

        # arules 'quality' slot is a data.frame
        private$quality <- arules_imp@quality

        name <- as.character(arules_imp@info$data)
        private$name <- name

        private$lhs_matrix <- as(arules_imp@lhs@data, "dgCMatrix")
        private$rhs_matrix <- as(arules_imp@rhs@data, "dgCMatrix")

        rownames(private$lhs_matrix) <- private$attributes
        rownames(private$rhs_matrix) <- private$attributes
      } else {
        args <- list(
          name = "",
          attributes = c(),
          lhs = NULL,
          rhs = NULL,
          I = NULL,
          quality = NULL,
          confidence = NULL
        ) # keep backward compat arg?
        args[names(dots)] <- dots

        private$name <- args$name
        private$attributes <- args$attributes
        private$lhs_matrix <- args$lhs
        private$rhs_matrix <- args$rhs
        private$I <- args$I

        if (!is.null(args$quality)) {
          private$quality <- args$quality
        } else {
          # Attempt to use confidence if provided, for backward compat
          if (!is.null(args$confidence)) {
            private$quality <- data.frame(confidence = args$confidence)
          } else {
            private$quality <- data.frame()
          }
        }
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
    #' Get a subset of the rule set
    #'
    #' @param idx   (integer or logical vector) Indices of the rules to extract or remove. If logical vector, only \code{TRUE} elements are retained and the rest discarded.
    #'
    #' @return A new \code{RuleSet} with only the rules given by the \code{idx} indices.
    #' @export
    `[` = function(idx) {
      if (is.logical(idx)) {
        idx <- which(idx)
      }

      idx <- idx[abs(idx) <= self$cardinality()]
      idx <- idx[abs(idx) > 0]

      if (length(idx) > 0) {
        if (all(idx < 0) | all(idx > 0)) {
          # Subset quality data frame
          new_quality <- private$quality[idx, , drop = FALSE]

          imp <- RuleSet$new(
            name = paste0(private$name, "_", paste0(idx)),
            attributes = private$attributes,
            lhs = Matrix::Matrix(private$lhs_matrix[, idx], sparse = TRUE),
            rhs = Matrix::Matrix(private$rhs_matrix[, idx], sparse = TRUE),
            quality = new_quality
          )

          return(imp)
        } else {
          stop(
            "Cannot use mixed positive and negative indices.\n",
            call. = FALSE
          )
        }
      } else {
        return(RuleSet$new(attributes = private$attributes, I = private$I))
      }
    },

    #' @description
    #' Convert to arules format
    #'
    #' @param quality   (logical) Compute/include the interest measures for each rule?
    #'
    #' @return A \code{rules} object as used by package \code{arules}.
    #'
    #' @import arules
    #' @importFrom methods as is
    #' @export
    to_arules = function(quality = TRUE) {
      if (self$is_empty()) {
        stop("No implications to export.\n", call. = FALSE)
      }

      if (!private$is_binary()) {
        stop(
          "Export to arules format is only allowed when using binary implications.\n",
          call. = FALSE
        )
      }

      # Needed to export to arules
      L <- .reduction(
        LHS = private$lhs_matrix,
        RHS = private$rhs_matrix,
        attributes = private$attributes
      )

      LHS <- as(L$lhs, "ngCMatrix")
      LHS <- as(LHS, "itemMatrix")
      itemLabels(LHS) <- private$attributes

      RHS <- as(L$rhs, "ngCMatrix")
      RHS <- as(RHS, "itemMatrix")
      itemLabels(RHS) <- private$attributes

      rules <- new("rules", lhs = LHS, rhs = RHS)

      if (quality) {
        # If we have stored quality, use it.
        # But maybe update it if I is provided?
        # For now, export what we have.
        if (nrow(private$quality) == length(rules)) {
          quality(rules) <- private$quality
        } else {
          # Only if quality matches rules count
        }
      }

      return(rules)
    },

    #' @description
    #' Add a precomputed rule set
    #'
    #' @param ...   An \code{RuleSet} object, or a pair \code{lhs}, \code{rhs} of \code{dgCMatrix}.
    #'
    #' @return Nothing, just updates the internal field.
    #'
    #' @import arules
    #' @export
    add = function(...) {
      dots <- list(...)

      # Just a single RuleSet
      if (length(dots) == 1) {
        if (inherits(dots[[1]], "RuleSet")) {
          private$append_implications(dots[[1]])
        } else {
          # It is a rules object
          implications <- RuleSet$new(dots[[1]])
          private$append_implications(implications)
        }
      } else {
        # It must come in the form LHS, RHS, quality
        if (length(dots) >= 2) {
          lhs <- dots[[1]]
          rhs <- dots[[2]]

          qual <- NULL
          if (length(dots) > 2) {
            qual <- dots[[3]]
          }

          # Helper to convert Set to vector removed for brevity if no Sets passed?
          # Assuming matrix passed for now based on RuleSet intended usage or kept robust.

          imp <- RuleSet$new(
            attributes = private$attributes,
            lhs = lhs,
            rhs = rhs,
            quality = qual
          )

          private$append_implications(imp)
        } else {
          stop("Invalid number of arguments.\n", call. = FALSE)
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
      lhs_size <- colSums(private$lhs_matrix)
      rhs_size <- colSums(private$rhs_matrix)

      return(cbind(LHS = lhs_size, RHS = rhs_size))
    },

    #' @description
    #' Print all rules to text
    #'
    #' @return A string with all the rules in the set.
    #'
    #' @export
    print = function() {
      if (is.null(private$lhs_matrix)) {
        cat("Rules set with 0 association rules.\n")

        return(invisible(self))
      }

      n_implications <- ncol(private$lhs_matrix)
      cat("Rules set with", n_implications, "rules\n")

      if (n_implications > 0) {
        attributes <- private$attributes
        LHS <- private$lhs_matrix
        RHS <- private$rhs_matrix

        implications <- sapply(
          seq_len(n_implications),
          function(i) {
            paste0(
              "Rule ",
              i,
              ": ",
              .implication_to_string(LHS[, i], RHS[, i], attributes)
            )
          }
        )

        # formatting quality
        if (nrow(private$quality) > 0) {
          # Format each row
          quals <- apply(private$quality, 1, function(row) {
            paste(names(row), "=", round(as.numeric(row), 2), collapse = ", ")
          })
          implications <- paste0(implications, " [", quals, "]")
        }

        implications <- sapply(implications, function(s) {
          stringr::str_wrap(s, width = 70, exdent = 2)
        })

        cat(implications, sep = "\n")
      }
    },

    #' @description
    #' Get quality metrics
    #'
    #' @return A data.frame with the quality metrics for each rule.
    #' @export
    get_quality = function() {
      return(private$quality)
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
    to_latex = function(
      print = TRUE,
      ncols = 1,
      numbered = TRUE,
      numbers = seq_len(self$cardinality())
    ) {
      output <- imp_to_latex(
        self,
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
        LHS <- Matrix::Matrix(
          FALSE,
          nrow = length(private$attributes),
          ncol = 1,
          sparse = TRUE
        )
      } else {
        LHS <- private$lhs_matrix
      }

      dimnames(LHS) <- list(private$attributes, paste0(seq_len(ncol(LHS))))

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
        RHS <- Matrix(
          FALSE,
          nrow = length(private$attributes),
          ncol = 1,
          sparse = TRUE
        )
      } else {
        RHS <- private$rhs_matrix
      }

      dimnames(RHS) <- list(private$attributes, paste0(seq_len(ncol(RHS))))

      return(RHS)
    },

    #' @description
    #' Filter implications by attributes in LHS and RHS
    #'
    #' @param lhs  (character vector) Names of the attributes to filter the LHS by. If \code{NULL}, no filtering is done on the LHS.
    #' @param rhs  (character vector) Names of the attributes to filter the RHS by. If \code{NULL}, no filtering is done on the RHS.
    #' @param drop  (logical) Remove the rest of attributes in RHS?
    #'
    #' @return An \code{RuleSet} that is a subset of the current set, only with those rules which has the attributes in \code{lhs} and \code{rhs} in their LHS and RHS, respectively.
    #'
    #' @export
    filter = function(lhs = NULL, rhs = NULL, drop = FALSE) {
      RHS <- private$rhs_matrix
      LHS <- private$lhs_matrix

      if (!is.null(lhs)) {
        # Filter the implications which have
        # the given lhs
        idx_attr <- match(lhs, private$attributes)

        if (length(idx_attr) > 1) {
          idx_lhs <- which(colSums(LHS[idx_attr, ]) > 0)
        } else {
          idx_lhs <- which(LHS[idx_attr, ] > 0)
        }
      } else {
        # If not specified a filter for LHS,
        # select all implications
        idx_lhs <- seq_len(ncol(LHS))
      }

      if (!is.null(rhs)) {
        # Filter the implications which have
        # the given lhs
        idx_attr <- match(rhs, private$attributes)

        if (length(idx_attr) > 1) {
          idx_rhs <- which(colSums(RHS[idx_attr, ]) > 0)
        } else {
          idx_rhs <- which(RHS[idx_attr, ] > 0)
        }
      } else {
        # If not specified a filter for RHS,
        # select all implications
        idx_rhs <- seq_len(ncol(RHS))
      }

      idx <- intersect(idx_lhs, idx_rhs)

      if (length(idx) == 0) {
        warning(
          "No combination of given LHS and RHS found.\n",
          call. = FALSE,
          immediate. = TRUE
        )
        return(invisible(NULL))
      }

      if (length(idx) > 0) {
        # New quality
        qual_new <- private$quality[idx, , drop = FALSE]

        if (drop && !is.null(rhs)) {
          newLHS <- LHS[, idx]
          newRHS <- RHS[, idx]

          other_idx <- setdiff(seq_len(nrow(RHS)), idx_attr)
          newRHS[other_idx, ] <- 0

          imp <- RuleSet$new(
            name = paste0(private$name, "_filtered"),
            attributes = private$attributes,
            lhs = Matrix(newLHS, sparse = TRUE),
            rhs = Matrix(newRHS, sparse = TRUE),
            quality = qual_new
          )
        } else {
          imp <- RuleSet$new(
            name = paste0(private$name, "_filtered"),
            attributes = private$attributes,
            lhs = Matrix(LHS[, idx], sparse = TRUE),
            rhs = Matrix(RHS[, idx], sparse = TRUE),
            quality = qual_new
          )
        }

        return(imp)
      }
    },

    get_implications = function() {
      # Confidence 1 implies implications?
      # If 'confidence' is in quality
      if ("confidence" %in% names(private$quality)) {
        idx <- which(private$quality$confidence == 1)
      } else {
        idx <- integer(0)
      }

      if (length(idx) > 0) {
        imp <- ImplicationSet$new(
          name = paste0(private$name, "_impl"),
          attributes = private$attributes,
          lhs = Matrix(private$lhs_matrix[, idx], sparse = TRUE),
          rhs = Matrix(private$rhs_matrix[, idx], sparse = TRUE)
        )

        return(imp)
      } else {
        warning("No implications found.", call. = FALSE)

        return(NULL)
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

      if ("support" %in% names(private$quality)) {
        return(private$quality$support)
      }

      # If not in quality, compute it?
      # But RuleSet might not have I (FormalContext).
      if (is.null(private$I)) {
        return(numeric(0))
      }

      subsets <- .subset(private$lhs_matrix, private$I)

      supp_val <- rowMeans(subsets)

      # Update quality?
      # private$quality$support <- supp_val

      return(supp_val)
    },

    confidence = function() {
      if (self$is_empty()) {
        return(numeric(0))
      }

      if ("confidence" %in% names(private$quality)) {
        return(private$quality$confidence)
      }

      if (is.null(private$I)) {
        return(numeric(0))
      }

      subsets <- .subset(private$lhs_matrix, private$I)

      supp_lhs <- rowMeans(subsets)

      XunionY <- .union(private$lhs_matrix, private$rhs_matrix)
      XYsupport <- rowMeans(.subset(XunionY, private$I))

      conf_val <- XYsupport / supp_lhs

      return(conf_val)
    }
  ),

  private = list(
    name = "",

    attributes = NULL,

    lhs_matrix = NULL,
    rhs_matrix = NULL,

    I = NULL,
    quality = NULL,
    binary = NULL,

    is_binary = function() {
      if (!is.null(private$binary)) {
        return(private$binary)
      }

      if (!is.null(private$I)) {
        v <- unique(c(0, private$I@x, 1))
      } else {
        if (!is.null(private$lhs_matrix)) {
          v <- unique(c(0, private$lhs_matrix@x, private$rhs_matrix@x, 1))
        } else {
          return(FALSE)
        }
      }

      private$binary <- (length(v) == 2) && all(v == c(0, 1))
      return(private$binary)
    },

    append_implications = function(implications) {
      LHS <- implications$get_LHS_matrix()
      RHS <- implications$get_RHS_matrix()
      q <- implications$get_quality()

      if (length(private$attributes) == nrow(LHS)) {
        private$lhs_matrix <- cbind(private$lhs_matrix, LHS)
        private$rhs_matrix <- cbind(private$rhs_matrix, RHS)

        # Append quality
        # Needed: align columns?
        # Assuming same structure allows rbind. If missing cols?

        # If private$quality is empty and q is not, take q?
        if (
          nrow(private$quality) == 0 &&
            nrow(q) > 0 &&
            ncol(private$lhs_matrix) == nrow(q)
        ) {
          private$quality <- q
        } else {
          if (ncol(private$quality) == ncol(q)) {
            private$quality <- rbind(private$quality, q)
          } else {
            # mismatch
            warning("Quality metrics mismatch. Filling with NA.")
            # Fill missing cols
            all_cols <- unique(c(names(private$quality), names(q)))
            q[setdiff(all_cols, names(q))] <- NA
            private$quality[setdiff(all_cols, names(private$quality))] <- NA
            private$quality <- rbind(private$quality, q)
          }
        }
      } else {
        stop("Dimensions mismatch.")
      }
    }
  )
)

# environment(RuleSet) <- asNamespace("fcaR")
