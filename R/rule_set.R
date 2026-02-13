#' @title
#' R6 class for a Rule Set
#'
#' @description
#' This class implements a generic rule set (LHS -> RHS), serving as the parent
#' class for \code{ImplicationSet}. It provides common functionality for
#' managing, filtering, and exporting rules.
#'
#' @importFrom Matrix colSums Matrix
#' @importFrom stringr str_wrap
#' @importFrom methods new
#'
#' @export
RuleSet <- R6::R6Class(
  "RuleSet",
  public = list(
    #' @description
    #' Initialize a RuleSet
    #'
    #' @param ...   A \code{rules} object (from \code{arules}) or named arguments:
    #' \code{name} (string), \code{attributes} (character vector),
    #' \code{lhs} and \code{rhs} (sparse matrices), \code{I} (incidence matrix),
    #' \code{quality} (data.frame), \code{confidence} (numeric vector, backward compat).
    #'
    #' @return A new \code{RuleSet} object.
    #'
    #' @export
    initialize = function(...) {
      check_needed_pkg("arules", "initialize from arules objects")
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
    #' @return A character vector with the names of the attributes used in the rules.
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

          imp <- private$create_subset(
            name = paste0(private$name, "_", paste0(idx)),
            attributes = private$attributes,
            lhs = Matrix::Matrix(private$lhs_matrix[, idx], sparse = TRUE),
            rhs = Matrix::Matrix(private$rhs_matrix[, idx], sparse = TRUE),
            I = private$I,
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
        return(private$create_subset(
          attributes = private$attributes,
          I = private$I
        ))
      }
    },

    #' @description
    #' Convert to arules format
    #'
    #' @param quality   (logical) Compute/include the interest measures for each rule?
    #'
    #' @return A \code{rules} object as used by package \code{arules}.
    #'
    #' @importFrom methods as is
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
        if (nrow(private$quality) == length(rules)) {
          quality(rules) <- private$quality
        } else if (!is.null(private$I)) {
          # Compute from incidence matrix
          rules <- imps_to_arules(
            LHS = private$lhs_matrix,
            RHS = private$rhs_matrix,
            attributes = private$attributes,
            I = private$I,
            quality = quality
          )
        }
      }

      return(rules)
    },

    #' @description
    #' Add a precomputed rule set
    #'
    #' @param ...   A \code{RuleSet} object, or a pair \code{lhs}, \code{rhs} of \code{dgCMatrix}.
    #'
    #' @return Nothing, just updates the internal field.
    #'
    #' @export
    add = function(...) {
      check_needed_pkg("arules", "import from arules objects")
      dots <- list(...)

      # Just a single RuleSet
      if (length(dots) == 1) {
        if (inherits(dots[[1]], "RuleSet")) {
          private$append_rules(dots[[1]])
        } else {
          # It is a rules object
          implications <- RuleSet$new(dots[[1]])
          private$append_rules(implications)
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

          imp <- RuleSet$new(
            attributes = private$attributes,
            lhs = lhs,
            rhs = rhs,
            quality = qual
          )

          private$append_rules(imp)
        } else {
          stop("Invalid number of arguments.\n", call. = FALSE)
        }
      }
    },

    #' @description
    #' Cardinality: Number of rules in the set
    #'
    #' @return The cardinality of the rule set.
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
    #' @return \code{TRUE} if the set of rules is empty, \code{FALSE} otherwise.
    #'
    #' @export
    is_empty = function() {
      is.null(private$lhs_matrix)
    },

    #' @description
    #' Size: number of attributes in each of LHS and RHS
    #'
    #' @return A matrix with two columns: the number of attributes present in each of the LHS and RHS of each rule.
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
      label <- private$rule_type_label()

      if (is.null(private$lhs_matrix)) {
        cat(label, "set with 0", paste0(label, ".\n"))
        return(invisible(self))
      }

      n_implications <- ncol(private$lhs_matrix)
      cat(label, "set with", n_implications, paste0(label, ".\n"))

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
          quals <- apply(private$quality, 1, function(row) {
            paste(names(row), "=", round(as.numeric(row), 2), collapse = ", ")
          })
          implications <- paste0(implications, " [", quals, "]")
        }

        implications <- sapply(implications, function(s) {
          stringr::str_wrap(s, width = getOption("width"), exdent = 2)
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
    #' @return A string in LaTeX format that prints nicely all the rules.
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
    #' @return A sparse matrix representing the LHS of the rules in the set.
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
    #' @return A sparse matrix representing the RHS of the rules in the set.
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
    #' Filter rules by attributes in LHS and RHS
    #'
    #' @param lhs  (character vector) Names of the attributes to filter the LHS by. If \code{NULL}, no filtering is done on the LHS.
    #' @param not_lhs  (character vector) Names of the attributes to not include in the LHS. If \code{NULL} (the default), it is not considered at all.
    #' @param rhs  (character vector) Names of the attributes to filter the RHS by. If \code{NULL}, no filtering is done on the RHS.
    #' @param not_rhs  (character vector) Names of the attributes to not include in the RHS. If \code{NULL} (the default), it is not considered at all.
    #' @param drop  (logical) Remove the rest of attributes in RHS?
    #'
    #' @return A \code{RuleSet} (or subclass) that is a subset of the current set, only with those rules which have the attributes in \code{lhs} and \code{rhs} in their LHS and RHS, respectively.
    #'
    #' @export
    filter = function(
      lhs = NULL,
      not_lhs = NULL,
      rhs = NULL,
      not_rhs = NULL,
      drop = FALSE
    ) {
      RHS <- private$rhs_matrix
      LHS <- private$lhs_matrix

      if (is.null(LHS) || ncol(LHS) == 0) {
        return(self)
      }

      if (!is.null(lhs)) {
        idx_attr <- match(lhs, private$attributes)

        if (length(idx_attr) > 1) {
          idx_lhs <- Matrix::which(Matrix::colSums(LHS[idx_attr, ]) > 0)
        } else {
          idx_lhs <- which(LHS[idx_attr, ] > 0)
        }
      } else {
        idx_lhs <- seq_len(ncol(LHS))
      }

      if (!is.null(not_lhs)) {
        idx_attr <- match(not_lhs, private$attributes)

        if (length(idx_attr) > 1) {
          idx_not_lhs <- Matrix::which(Matrix::colSums(LHS[idx_attr, ]) > 0)
        } else {
          idx_not_lhs <- which(LHS[idx_attr, ] > 0)
        }
      } else {
        idx_not_lhs <- c()
      }

      idx_lhs <- setdiff(idx_lhs, idx_not_lhs)

      if (!is.null(rhs)) {
        idx_attr <- match(rhs, private$attributes)

        if (length(idx_attr) > 1) {
          idx_rhs <- Matrix::which(Matrix::colSums(RHS[idx_attr, ]) > 0)
        } else {
          idx_rhs <- which(RHS[idx_attr, ] > 0)
        }
      } else {
        idx_rhs <- seq_len(ncol(RHS))
      }

      if (!is.null(not_rhs)) {
        idx_attr <- match(not_rhs, private$attributes)

        if (length(idx_attr) > 1) {
          idx_not_rhs <- Matrix::which(Matrix::colSums(RHS[idx_attr, ]) > 0)
        } else {
          idx_not_rhs <- which(RHS[idx_attr, ] > 0)
        }
      } else {
        idx_not_rhs <- c()
      }

      idx_rhs <- setdiff(idx_rhs, idx_not_rhs)

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
        # Subset quality
        qual_new <- private$quality[idx, , drop = FALSE]

        if (drop && !is.null(rhs)) {
          newLHS <- LHS[, idx]
          newRHS <- RHS[, idx]

          other_idx <- setdiff(
            seq_len(nrow(RHS)),
            match(rhs, private$attributes)
          )
          newRHS[other_idx, ] <- 0

          imp <- private$create_subset(
            name = paste0(private$name, "_filtered"),
            attributes = private$attributes,
            lhs = Matrix::Matrix(newLHS, sparse = TRUE),
            rhs = Matrix::Matrix(newRHS, sparse = TRUE),
            I = private$I,
            quality = qual_new
          )
        } else {
          imp <- private$create_subset(
            name = paste0(private$name, "_filtered"),
            attributes = private$attributes,
            lhs = Matrix::Matrix(LHS[, idx], sparse = TRUE),
            rhs = Matrix::Matrix(RHS[, idx], sparse = TRUE),
            I = private$I,
            quality = qual_new
          )
        }

        return(imp)
      }
    },

    #' @description
    #' Extract the implications with confidence 1
    #'
    #' @return An \code{ImplicationSet} object containing only the rules with confidence 1.
    #' @export
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
    #' Compute support of each rule
    #'
    #' @return A vector with the support of each rule.
    #' @export
    support = function() {
      if (self$is_empty()) {
        return(numeric(0))
      }

      if ("support" %in% names(private$quality)) {
        return(private$quality$support)
      }

      # If not in quality, compute it from I
      if (is.null(private$I)) {
        return(numeric(0))
      }

      subsets <- .subset(private$lhs_matrix, private$I)

      supp_val <- Matrix::rowMeans(subsets)

      return(supp_val)
    },

    #' @description
    #' Compute the confidence of each rule
    #'
    #' @return A numeric vector with the confidence of each rule.
    #' @export
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
    },

    #' @description
    #' Export the rule set to JSON
    #'
    #' @param file        (character) The path of the file to save the JSON to.
    #' @param return_list (logical) If TRUE, returns the list representation instead of the JSON string.
    #'
    #' @return A JSON string representing the rule set, or a list if \code{return_list} is TRUE.
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

      is_bin <- private$is_binary()

      mat_to_list <- function(M, attrs) {
        # Force conversion to general triplet
        T <- as(as(M, "dgCMatrix"), "dgTMatrix")

        idx_available <- (length(T@x) > 0)

        if (!idx_available) {
          return(replicate(ncol(M), list()))
        }

        vals <- if (.hasSlot(T, "x")) T@x else rep(1, length(T@i))

        df_all <- data.frame(
          attr = attrs[T@i + 1],
          val = vals,
          rule = T@j + 1
        )

        res <- split(df_all, factor(df_all$rule, levels = seq_len(ncol(M))))
        lapply(res, function(df) {
          if (nrow(df) == 0) {
            return(list())
          }
          if (is_bin) {
            return(as.list(df$attr))
          } else {
            out <- as.list(df$val)
            names(out) <- df$attr
            return(out)
          }
        })
      }

      lhs_data <- mat_to_list(private$lhs_matrix, private$attributes)
      rhs_data <- mat_to_list(private$rhs_matrix, private$attributes)

      out <- list(
        type = private$json_type_name(),
        attributes = private$attributes,
        rules = lapply(seq_len(self$cardinality()), function(i) {
          r <- list(
            lhs = lhs_data[[i]],
            rhs = rhs_data[[i]]
          )
          # Add extra quality measures if available
          if (length(private$quality) > 0 && nrow(private$quality) >= i) {
            q <- as.list(private$quality[i, , drop = FALSE])
            r <- c(r, q)
          }
          return(r)
        })
      )

      if (return_list) {
        return(out)
      }

      json <- jsonlite::toJSON(out, auto_unbox = TRUE)
      if (!is.null(file)) {
        writeLines(json, file)
        return(invisible(json))
      }
      return(json)
    }
  ),

  private = list(
    name = "",

    attributes = NULL,

    lhs_matrix = NULL,
    rhs_matrix = NULL,
    I = NULL,
    quality = NULL,

    is_binary = function() {
      if (is.null(private$lhs_matrix)) {
        return(FALSE)
      }
      v <- unique(c(0, private$lhs_matrix@x, 1))
      return(length(v) == 2 && all(v == c(0, 1)))
    },

    # Virtual-like helpers for subclass customization
    rule_type_label = function() "Rules",
    json_type_name = function() "RuleSet",

    # Factory method for subsetting — subclasses override to return their own type
    create_subset = function(...) {
      RuleSet$new(...)
    },

    append_rules = function(rules_obj) {
      # Valid checks... omitted for brevity
      LHS <- rules_obj$get_LHS_matrix()
      RHS <- rules_obj$get_RHS_matrix()

      if (length(private$attributes) == nrow(LHS)) {
        private$lhs_matrix <- cbind(private$lhs_matrix, LHS)
        private$rhs_matrix <- cbind(private$rhs_matrix, RHS)

        # Merge quality?
        q1 <- private$quality
        q2 <- rules_obj$get_quality()

        # If one is empty and the other not, we obtain a problem.
        # We try to merge by name.
        if (nrow(q1) > 0 && nrow(q2) > 0) {
          # intersect cols
          cols <- intersect(names(q1), names(q2))
          if (length(cols) > 0) {
            private$quality <- rbind(
              q1[, cols, drop = FALSE],
              q2[, cols, drop = FALSE]
            )
          } else {
            # No common cols
            private$quality <- data.frame()
          }
        } else {
          # If one is empty, result is empty usually unless we fill with NA
          private$quality <- data.frame()
        }
      } else {
        stop("Dimensions mismatch.")
      }
    }
  )
)
#' @title Import RuleSet from JSON
#' @description Reconstructs a RuleSet object from a JSON string.
#' @param json_str A JSON string generated by \code{to_json()}.
#' @return A \code{RuleSet} object.
#' @export
rules_from_json <- function(json_str) {
  check_needed_pkg("jsonlite", "import from JSON")

  data <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)

  if (data$type != "RuleSet") {
    stop("Invalid JSON: type must be 'RuleSet'")
  }

  attributes <- unlist(data$attributes)
  n_rules <- length(data$rules)

  if (n_rules == 0) {
    return(RuleSet$new(attributes = attributes))
  }

  parse_part <- function(part_list) {
    i <- integer(0)
    j <- integer(0)
    x <- numeric(0)

    for (rule_idx in seq_along(part_list)) {
      item <- part_list[[rule_idx]]
      if (length(item) > 0) {
        if (!is.null(names(item))) {
          attrs <- names(item)
          vals <- unlist(item)
        } else {
          attrs <- unlist(item)
          vals <- rep(1, length(attrs))
        }
        attr_idxs <- match(attrs, attributes)
        if (any(is.na(attr_idxs))) {
          warning(sprintf(
            "Unknown attributes in rule %d: %s",
            rule_idx,
            paste(attrs[is.na(attr_idxs)], collapse = ", ")
          ))
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

  # Extract quality metrics
  quality_cols <- setdiff(
    unique(unlist(lapply(data$rules, names))),
    c("lhs", "rhs")
  )

  quality <- data.frame()
  if (length(quality_cols) > 0) {
    quality <- as.data.frame(
      lapply(quality_cols, function(col) {
        sapply(data$rules, function(r) {
          v <- r[[col]]
          if (is.null(v)) NA else v
        })
      })
    )
    names(quality) <- quality_cols
  }

  RS <- RuleSet$new(
    attributes = attributes,
    lhs = LHS,
    rhs = RHS,
    quality = quality
  )

  return(RS)
}
