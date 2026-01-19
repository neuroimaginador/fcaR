#' @title dplyr verbs for ImplicationSet
#' @name dplyr_implications
#' @description S3 methods to enable dplyr verbs on ImplicationSet objects.
#'
#' @importFrom dplyr filter arrange slice
#' @importFrom rlang enquo enquos eval_tidy new_data_mask
#' @importFrom Matrix colSums
#'
#' @param .data An object of class \code{ImplicationSet}.
#' @param ...   Expressions used for filtering, arranging, or slicing.
#'
#' @export
arrange.ImplicationSet <- function(.data, ...) {
  dots <- rlang::enquos(...)
  if (length(dots) == 0) return(.data)

  df_metrics <- .get_implication_metrics(.data)
  df_arranged <- dplyr::arrange(df_metrics, !!!dots)

  # Índices ordenados según el criterio de dplyr
  new_order_idx <- as.integer(df_arranged$tmp_rule_id)

  # # FIX: Reconstruir manualmente el objeto.
  # # El operador `[` interno de fcaR suele reordenar índices (1, 2, ...).
  # # Al extraer las matrices con R y crear un objeto nuevo, forzamos el orden deseado.
  # lhs <- .data$get_LHS_matrix()[, new_order_idx, drop = FALSE]
  # rhs <- .data$get_RHS_matrix()[, new_order_idx, drop = FALSE]
  #
  # # Creamos el nuevo set con las matrices ya ordenadas
  # new_set <- ImplicationSet$new(attributes = .data$get_attributes(),
  #                               lhs = lhs,
  #                               rhs = rhs)

  new_set <- .data$clone()[new_order_idx]
  return(new_set)
}

#' @export
slice.ImplicationSet <- function(.data, ...) {
  n_rules <- .data$cardinality()
  if (n_rules == 0) return(.data)

  dummy_df <- data.frame(tmp_rule_id = seq_len(n_rules))
  sliced_df <- dplyr::slice(dummy_df, ...)

  if (nrow(sliced_df) == 0) {
    # Devolver set vacío
    return(ImplicationSet$new(attributes = .data$get_attributes()))
  }

  indices <- unname(as.integer(sliced_df$tmp_rule_id))

  new_set <- .data[indices]

  # FIX: Reconstruir manualmente para permitir reordenación (ej. slice(10:1))
  # lhs <- .data$get_LHS_matrix()[, indices, drop = FALSE]
  # rhs <- .data$get_RHS_matrix()[, indices, drop = FALSE]
  #
  # new_set <- ImplicationSet$new(attributes = .data$get_attributes(),
  #                               lhs = lhs,
  #                               rhs = rhs)
  return(new_set)
}

#' @export
filter.ImplicationSet <- function(.data, ...) {
  dots <- rlang::enquos(...)
  if (length(dots) == 0) return(.data)

  df_metrics <- .get_implication_metrics(.data)

  check_attrs <- function(matrix_accessor, type = "has") {
    mat <- matrix_accessor()
    all_attrs <- .data$get_attributes()

    function(...) {
      query_attrs <- c(...)
      valid_attrs <- intersect(query_attrs, all_attrs)

      if (length(valid_attrs) == 0) {
        if (length(query_attrs) > 0) warning("Some requested attributes do not exist.")
        if (type == "lacks") return(rep(TRUE, ncol(mat)))
        return(rep(FALSE, ncol(mat)))
      }

      idx <- match(valid_attrs, all_attrs)
      sub_mat <- mat[idx, , drop = FALSE]
      counts <- Matrix::colSums(sub_mat)

      if (type == "has") return(counts == length(idx))
      else if (type == "lacks") return(counts == 0)
      else if (type == "any") return(counts > 0)
    }
  }

  mask <- rlang::new_data_mask(as.environment(df_metrics))

  mask$lhs_has   <- check_attrs(.data$get_LHS_matrix, "has")
  mask$lhs_lacks <- check_attrs(.data$get_LHS_matrix, "lacks")
  mask$lhs_any   <- check_attrs(.data$get_LHS_matrix, "any")
  mask$rhs_has   <- check_attrs(.data$get_RHS_matrix, "has")
  mask$rhs_lacks <- check_attrs(.data$get_RHS_matrix, "lacks")
  mask$rhs_any   <- check_attrs(.data$get_RHS_matrix, "any")

  mask$lhs     <- mask$lhs_has
  mask$rhs     <- mask$rhs_has
  mask$not_lhs <- mask$lhs_lacks
  mask$not_rhs <- mask$rhs_lacks

  final_logical <- rep(TRUE, .data$cardinality())

  for (quo in dots) {
    res <- rlang::eval_tidy(quo, data = mask)
    final_logical <- final_logical & res
  }

  indices <- which(final_logical)

  new_set <- .data[final_logical]

  # Para filter, el orden relativo no cambia (1, 2, 5...), por lo que usar
  # reconstrucción es seguro y consistente con arrange/slice.
  # lhs <- .data$get_LHS_matrix()[, indices, drop = FALSE]
  # rhs <- .data$get_RHS_matrix()[, indices, drop = FALSE]
  #
  # new_set <- ImplicationSet$new(attributes = .data$get_attributes(),
  #                               lhs = lhs,
  #                               rhs = rhs)
  return(new_set)
}

#' @export
mutate.ImplicationSet <- function(.data, ...) {
  stop("mutate() is not supported for ImplicationSet objects directly.", call. = FALSE)
}

# --- Internal Helper ---
.get_implication_metrics <- function(imps) {
  lhs_mat <- imps$get_LHS_matrix()
  rhs_mat <- imps$get_RHS_matrix()
  lhs_s <- Matrix::colSums(lhs_mat)
  rhs_s <- Matrix::colSums(rhs_mat)
  supp <- imps$support()

  df <- data.frame(
    tmp_rule_id = seq_len(imps$cardinality()),
    support = supp,
    lhs_size = lhs_s,
    rhs_size = rhs_s,
    size = lhs_s + rhs_s
  )
  return(df)
}
