#' Check if a formal context is many-valued
#' @param I A formal context (incidence matrix).
#' @return TRUE if the context is many-valued, FALSE otherwise.
#' @importFrom methods .hasSlot
#' @noRd
check_many_valued <- function(I) {
  if (
    inherits(I, "sparseMatrix") ||
      inherits(I, "CsparseMatrix") ||
      inherits(I, "TsparseMatrix")
  ) {
    if (!.hasSlot(I, "x")) {
      return(FALSE)
    }
    val <- if (.hasSlot(I, "x")) I@x else numeric(0)
  } else {
    val <- as.vector(as.matrix(I))
  }

  # Suppress warnings about NAs introduced by coercion
  suppressWarnings(
    val_num <- as.numeric(val)
  )

  # If coercion introduced NAs (where original wasn't NA), then it's many-valued (categorical)
  is_coercion_failure <- is.na(val_num) & !is.na(val)

  if (any(is_coercion_failure)) {
    return(TRUE)
  }

  # Filter out all NAs
  val_num <- na.omit(val_num)

  if (length(val_num) == 0) {
    return(FALSE)
  }

  return(any(val_num > 1 | val_num < 0))
}

error_many_valued <- function() {
  stop(
    "This formal context is many-valued, and this operation needs it to be binary or fuzzy.",
    call. = FALSE
  )
}
