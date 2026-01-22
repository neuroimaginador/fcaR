#' @title dplyr verbs for FormalContext
#' @name dplyr_verbs
#' @description S3 methods to enable dplyr verbs on FormalContext objects.
#'
#' @importFrom dplyr select filter mutate arrange rename
#' @importFrom tidyselect eval_select eval_rename
#' @importFrom rlang enquo enquos expr
#' @importFrom methods as
#' @importFrom Matrix Matrix
#' @importFrom stats setNames
#'
#' @param .data An object of class \code{FormalContext}.
#' @param ...   Arguments passed to the corresponding dplyr verbs.
#'
#' @export
select.FormalContext <- function(.data, ...) {
  current_attrs <- .data$attributes
  sim_data <- setNames(seq_along(current_attrs), current_attrs)

  selected_pos <- tidyselect::eval_select(
    rlang::expr(c(...)),
    data = sim_data
  )

  selected_names <- names(selected_pos)
  return(.data$subcontext(attributes = selected_names))
}

#' @export
filter.FormalContext <- function(.data, ...) {
  dots <- rlang::enquos(...)
  if (length(dots) == 0) return(.data)

  # Robust conversion: ensure we have a dense matrix for data.frame creation
  # This handles both 'matrix' and 'dgCMatrix' (sparse) outputs from incidence()
  inc_mat <- as.matrix(.data$incidence())
  df <- as.data.frame(inc_mat)
  df$`_row_id_` <- seq_len(nrow(df))

  filtered_df <- dplyr::filter(df, !!!dots)

  # Handle empty result (User's fix)
  if (nrow(filtered_df) == 0) {
    new_fc <- FormalContext$new()
    new_fc$attributes <- .data$attributes

    # Try to preserve logic if possible
    try({
      if (!is.null(.data$get_logic)) new_fc$use_logic(.data$get_logic())
    }, silent = TRUE)

    return(new_fc)
  }

  keep_idx <- filtered_df$`_row_id_`
  kept_objects <- .data$objects[keep_idx]

  return(.data$subcontext(objects = kept_objects))
}

#' @export
mutate.FormalContext <- function(.data, ...) {
  dots <- rlang::enquos(...)
  if (length(dots) == 0) return(.data)

  inc_mat <- as.matrix(.data$incidence())
  df <- as.data.frame(inc_mat)

  df_mutated <- dplyr::mutate(df, !!!dots)

  new_fc <- FormalContext$new(df_mutated)

  try({
    if (!is.null(.data$get_logic)) new_fc$use_logic(.data$get_logic())
  }, silent = TRUE)

  return(new_fc)
}

#' @export
arrange.FormalContext <- function(.data, ...) {
  dots <- rlang::enquos(...)
  if (length(dots) == 0) return(.data)

  inc_mat <- as.matrix(.data$incidence())
  df <- as.data.frame(inc_mat)
  df$`_row_id_` <- seq_len(nrow(df))

  df_arranged <- dplyr::arrange(df, !!!dots)

  if (nrow(df_arranged) == 0) {
    new_fc <- FormalContext$new()
    new_fc$attributes <- .data$attributes
    return(new_fc)
  }

  new_order_idx <- df_arranged$`_row_id_`
  ordered_objects <- .data$objects[new_order_idx]

  return(.data$subcontext(objects = ordered_objects))
}

#' @export
#' @importFrom stats setNames
rename.FormalContext <- function(.data, ...) {
  current_attrs <- .data$attributes
  sim_data <- setNames(seq_along(current_attrs), current_attrs)

  renamed_pos <- tidyselect::eval_rename(
    rlang::expr(c(...)),
    data = sim_data
  )

  if (length(renamed_pos) == 0) return(.data)

  new_fc <- .data$clone(deep = TRUE)

  new_names <- names(renamed_pos)
  old_indices <- as.integer(renamed_pos)

  new_fc$attributes[old_indices] <- new_names

  # Update internal matrix names safely
  try({
    if (!is.null(rownames(new_fc$I)) && length(rownames(new_fc$I)) == length(new_fc$attributes)) {
      rownames(new_fc$I) <- new_fc$attributes
    } else if (!is.null(colnames(new_fc$I)) && length(colnames(new_fc$I)) == length(new_fc$attributes)) {
      colnames(new_fc$I) <- new_fc$attributes
    }
  }, silent = TRUE)

  return(new_fc)
}
