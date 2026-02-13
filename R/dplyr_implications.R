#' @title dplyr verbs for RuleSet
#' @name dplyr_rules
#' @description S3 methods to enable dplyr verbs on RuleSet (and ImplicationSet) objects.
#'
#' @importFrom dplyr filter arrange slice
#' @importFrom rlang enquo enquos eval_tidy new_data_mask
#' @importFrom Matrix colSums
#'
#' @param .data An object of class \code{RuleSet} or \code{ImplicationSet}.
#' @param ...   Expressions used for filtering, arranging, or slicing.
#'
#' @export
arrange.RuleSet <- function(.data, ...) {
  dots <- rlang::enquos(...)
  if (length(dots) == 0) {
    return(.data)
  }

  df_metrics <- .get_rule_metrics(.data)
  df_arranged <- dplyr::arrange(df_metrics, !!!dots)

  # Índices ordenados según el criterio de dplyr
  new_order_idx <- as.integer(df_arranged$tmp_rule_id)

  new_set <- .data$clone()[new_order_idx]
  return(new_set)
}

#' @export
slice.RuleSet <- function(.data, ...) {
  n_rules <- .data$cardinality()
  if (n_rules == 0) {
    return(.data)
  }

  dummy_df <- data.frame(tmp_rule_id = seq_len(n_rules))
  sliced_df <- dplyr::slice(dummy_df, ...)

  if (nrow(sliced_df) == 0) {
    # Return empty set of the same class
    cls <- class(.data)[1]
    return(get(cls)$new(attributes = .data$get_attributes()))
  }

  indices <- unname(as.integer(sliced_df$tmp_rule_id))

  new_set <- .data[indices]

  return(new_set)
}

#' @export
filter.RuleSet <- function(.data, ...) {
  dots <- rlang::enquos(...)
  if (length(dots) == 0) {
    return(.data)
  }

  df_metrics <- .get_rule_metrics(.data)

  check_attrs <- function(matrix_accessor, type = "has") {
    mat <- matrix_accessor()
    all_attrs <- .data$get_attributes()

    function(...) {
      query_attrs <- c(...)
      valid_attrs <- intersect(query_attrs, all_attrs)

      if (length(valid_attrs) == 0) {
        if (length(query_attrs) > 0) {
          warning("Some requested attributes do not exist.")
        }
        if (type == "lacks") {
          return(rep(TRUE, ncol(mat)))
        }
        return(rep(FALSE, ncol(mat)))
      }

      idx <- match(valid_attrs, all_attrs)
      sub_mat <- mat[idx, , drop = FALSE]
      counts <- Matrix::colSums(sub_mat)

      if (type == "has") {
        return(counts == length(idx))
      } else if (type == "lacks") {
        return(counts == 0)
      } else if (type == "any") {
        return(counts > 0)
      }
    }
  }

  mask <- rlang::new_data_mask(as.environment(df_metrics))

  mask$lhs_has <- check_attrs(.data$get_LHS_matrix, "has")
  mask$lhs_lacks <- check_attrs(.data$get_LHS_matrix, "lacks")
  mask$lhs_any <- check_attrs(.data$get_LHS_matrix, "any")
  mask$rhs_has <- check_attrs(.data$get_RHS_matrix, "has")
  mask$rhs_lacks <- check_attrs(.data$get_RHS_matrix, "lacks")
  mask$rhs_any <- check_attrs(.data$get_RHS_matrix, "any")

  mask$lhs <- mask$lhs_has
  mask$rhs <- mask$rhs_has
  mask$not_lhs <- mask$lhs_lacks
  mask$not_rhs <- mask$rhs_lacks

  final_logical <- rep(TRUE, .data$cardinality())

  for (quo in dots) {
    res <- rlang::eval_tidy(quo, data = mask)
    final_logical <- final_logical & res
  }

  indices <- which(final_logical)

  new_set <- .data[final_logical]

  return(new_set)
}

#' @export
mutate.RuleSet <- function(.data, ...) {
  stop("mutate() is not supported for RuleSet objects directly.", call. = FALSE)
}

# --- Internal Helper ---
.get_rule_metrics <- function(rules) {
  lhs_mat <- rules$get_LHS_matrix()
  rhs_mat <- rules$get_RHS_matrix()
  lhs_s <- Matrix::colSums(lhs_mat)
  rhs_s <- Matrix::colSums(rhs_mat)
  supp <- rules$support()

  df <- data.frame(
    tmp_rule_id = seq_len(rules$cardinality()),
    support = supp,
    lhs_size = lhs_s,
    rhs_size = rhs_s,
    size = lhs_s + rhs_s
  )
  return(df)
}
