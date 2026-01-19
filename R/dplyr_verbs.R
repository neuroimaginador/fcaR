# -------------------------------------------------------------------------
# dplyr::select method for FormalContext
# -------------------------------------------------------------------------

#' Select attributes from a FormalContext
#'
#' @param .data An object of class \code{FormalContext}.
#' @param ... Expressions to select attributes (columns), as in \code{dplyr::select}.
#'
#' @return A new \code{FormalContext} subsetted by attributes.
#' @importFrom dplyr filter select
#' @importFrom tidyselect eval_select
#' @importFrom rlang enquo eval_tidy as_label
#' @export
select.FormalContext <- function(.data, ...) {

  # 1. Get current attributes
  current_attrs <- .data$attributes

  # 2. Create a named vector for tidyselect to work against
  #    tidyselect needs a 'data' structure to match names.
  #    We simulate the column names.
  sim_data <- setNames(seq_along(current_attrs), current_attrs)

  # 3. Use tidyselect::eval_select to resolve the (...) expressions
  #    This handles: select(fc, starts_with("A")), select(fc, 1:3), etc.
  selected_pos <- tidyselect::eval_select(
    rlang::expr(c(...)),
    data = sim_data
  )

  # 4. Extract names of selected attributes
  selected_names <- names(selected_pos)

  # 5. Return the subcontext
  #    Using the existing subcontext method of the R6 class
  return(.data$subcontext(attributes = selected_names))
}

# -------------------------------------------------------------------------
# dplyr::filter method for FormalContext
# -------------------------------------------------------------------------

#' Filter objects from a FormalContext
#'
#' @param .data An object of class \code{FormalContext}.
#' @param ... Logical predicates defined in terms of the attributes (columns).
#'
#' @details
#' This method allows filtering objects (rows) based on the values of their attributes.
#' Note: Since FormalContext stores data sparsely, this method internally densifies
#' the necessary columns to evaluate the conditions, which might be memory intensive
#' for very large contexts.
#'
#' @return A new \code{FormalContext} subsetted by objects.
#' @export
filter.FormalContext <- function(.data, ...) {

  # 1. Capture the filtering expressions (quosures)
  dots <- rlang::enquos(...)

  if (length(dots) == 0) {
    return(.data)
  }

  # 2. We need a data frame context to evaluate the expressions.
  #    Converting the WHOLE sparse matrix to a data frame is expensive.
  #    Optimization: We could try to identify which columns are needed,
  #    but for simplicity and robustness, we convert to a dense data frame.
  #    (Alternative: Create a wrapper environment if memory is critical).

  #    Let's extract the incidence matrix as a data.frame for evaluation
  df <- as.data.frame(as.matrix(.data$incidence()))

  # 3. Apply standard dplyr::filter on this data frame to find matching rows
  #    We add a temporary ID column to track the rows
  df$`_row_id_` <- seq_len(nrow(df))

  filtered_df <- dplyr::filter(df, !!!dots)

  # 4. Get the indices of the remaining objects
  keep_idx <- filtered_df$`_row_id_`

  # 5. Return the subcontext
  #    Using the existing subcontext method of the R6 class.
  #    If keep_idx is empty, subcontext handles it (returns empty context).

  kept_objects <- .data$objects[keep_idx]

  return(.data$subcontext(objects = kept_objects))
}
