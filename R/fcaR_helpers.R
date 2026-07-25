#' Build an attribute \code{Set} from names
#'
#' Replaces the common two-step pattern of creating and assigning attributes manually
#' with a single call. Useful for implication closures and \code{$recommend()}.
#'
#' @param fc A \code{FormalContext}.
#' @param ... Attribute names: a character vector, or several strings.
#'
#' @return A \code{Set} over \code{fc$attributes}.
#'
#' @examples
#' \dontrun{
#' S <- attribute_set(fc, "Romantic", "Slow_burn")
#' fc$implications$closure(S)
#' }
#'
#' @export
attribute_set <- function(fc, ...) {
  .check_formal_context(fc)
  attributes <- .collect_names(..., universe = fc$attributes, what = "attribute")
  .set_from_names(fc$attributes, attributes)
}

#' Build an object \code{Set} from names
#'
#' Dual of \code{attribute_set()}: builds a \code{Set} over \code{fc$objects}.
#'
#' @param fc A \code{FormalContext}.
#' @param ... Object names: a character vector, or several strings.
#'
#' @return A \code{Set} over \code{fc$objects}.
#'
#' @examples
#' \dontrun{
#' object_set(fc, "Squid Game", "Dark")
#' }
#'
#' @export
object_set <- function(fc, ...) {
  .check_formal_context(fc)
  objects <- .collect_names(..., universe = fc$objects, what = "object")
  .set_from_names(fc$objects, objects)
}

#' Pretty table of recommendation scores
#'
#' Turns the named numeric vector from \code{implications$recommend()} into a
#' sorted data frame that is easy to read in the console or in Quarto.
#'
#' @param recoms Named numeric vector (as returned by \code{$recommend()}).
#' @param drop_zero Logical; if \code{TRUE} (default), drop attributes with
#'   score 0 (not implied / not recommended).
#' @param query Optional character vector of attributes that were in the original
#'   query. If set, adds a \code{role} column: \code{"query"},
#'   \code{"recommended"}, or \code{"not implied"}.
#'
#' @return A data frame with columns \code{attribute}, \code{score}, and
#'   optionally \code{role}. Rows are ordered by decreasing score, then name.
#'
#' @examples
#' \dontrun{
#' S <- attribute_set(fc, "Romantic", "Slow_burn")
#' recoms <- fc$implications$recommend(S, attribute_filter = fc$attributes)
#' recommendation_table(recoms)
#' }
#'
#' @export
recommendation_table <- function(recoms, drop_zero = TRUE, query = NULL) {
  if (is.null(names(recoms)) || length(recoms) == 0L) {
    stop("`recoms` must be a named numeric vector from $recommend().", call. = FALSE)
  }

  df <- data.frame(
    attribute = names(recoms),
    score     = as.numeric(recoms),
    stringsAsFactors = FALSE
  )

  if (!is.null(query)) {
    query <- as.character(query)
    df$role <- ifelse(
      df$attribute %in% query,
      "query",
      ifelse(df$score > 0, "recommended", "not implied")
    )
    df$role <- factor(df$role, levels = c("query", "recommended", "not implied"))
  }

  if (isTRUE(drop_zero)) {
    df <- df[df$score > 0, , drop = FALSE]
  }

  ord <- order(-df$score, df$attribute)
  df <- df[ord, , drop = FALSE]
  rownames(df) <- NULL
  df
}

#' Iterative closure recommender (Simplification Logic)
#'
#' Multi-round attribute recommender: at each step computes
#' \code{closure(S, reduce = TRUE)} (infer + prune the rule-base), then asks
#' about the still-unknown attribute that appears most often in the LHS of the
#' *active* implications.
#'
#' @param fc A \code{FormalContext} with implications already computed.
#' @param initial Character vector of attribute names known at the start.
#' @param max_rounds Maximum number of question rounds.
#' @param interactive_mode If \code{TRUE} and the session is interactive, ask
#'   Yes/No via \code{menu()}; otherwise auto-accept the top suggestion.
#' @param verbose Print progress each round.
#'
#' @return Invisibly, a list with \code{query} (final \code{Set}) and
#'   \code{implications} (the last pruned implication set).
#'
#' @export
iterative_recommender <- function(fc,
                                  initial = character(0),
                                  max_rounds = 4,
                                  interactive_mode = FALSE,
                                  verbose = TRUE) {
  .check_formal_context(fc)

  if (is.null(fc$implications) || fc$implications$cardinality() == 0) {
    stop("No implications found. Call fc$find_implications() first.", call. = FALSE)
  }

  attrs <- fc$attributes
  if (length(initial) > 0L) {
    initial <- as.character(initial)
    unknown <- setdiff(initial, attrs)
    if (length(unknown) > 0L) {
      stop(
        "Unknown attribute(s) in `initial`: ", paste(unknown, collapse = ", "),
        call. = FALSE
      )
    }
  }

  x <- rep(0, length(attrs))
  names(x) <- attrs
  if (length(initial) > 0L) {
    x[initial] <- 1
  }

  active_imps <- fc$implications$clone()
  round <- 0L

  for (round in seq_len(as.integer(max_rounds))) {
    S      <- fcaR::as_Set(x)
    result <- active_imps$closure(S, reduce = TRUE)

    x           <- as.numeric(fcaR::as_vector(result$closure))
    names(x)    <- attrs
    active_imps <- result$implications

    remaining <- names(x)[x < 1]

    if (isTRUE(verbose)) {
      cat(sprintf(
        "Round %d - attributes known: %d/%d | active rules left: %d\n",
        round, sum(x > 0), length(attrs), active_imps$cardinality()
      ))
    }

    if (length(remaining) == 0L || active_imps$cardinality() == 0) {
      if (isTRUE(verbose)) {
        cat("  -> Closure is stable (a formal concept). Stopping.\n")
      }
      break
    }

    lhs_counts <- Matrix::rowSums(active_imps$get_LHS_matrix())
    names(lhs_counts) <- attrs
    candidate_counts  <- lhs_counts[remaining]
    best_att <- names(sort(candidate_counts, decreasing = TRUE))[1]

    if (is.na(best_att) || candidate_counts[best_att] <= 0) {
      if (isTRUE(verbose)) {
        cat("  -> No attribute triggers further deductions. Stopping.\n")
      }
      break
    }

    if (isTRUE(interactive_mode) && interactive()) {
      answer <- utils::menu(
        c("Yes", "No"),
        title = sprintf("Round %d - should the series have '%s'?", round, best_att)
      )
      value <- if (answer == 1) 1 else 0
    } else {
      value <- 1
    }

    if (isTRUE(verbose)) {
      cat(sprintf(
        "  -> Top question: '%s' (in %d active rules) -> answer: %s\n",
        best_att, candidate_counts[best_att],
        ifelse(value == 1, "yes", "no")
      ))
    }

    x[best_att] <- value
  }

  final_query <- fcaR::as_Set(x)
  if (isTRUE(verbose)) {
    cat("\nFinal profile after", round, "round(s):\n")
    print(final_query)
  }

  invisible(list(query = final_query, implications = active_imps))
}

# ---- internal helpers --------------------------------------------------------

#' @keywords internal
.check_formal_context <- function(fc) {
  if (!inherits(fc, "FormalContext")) {
    stop("`fc` must be a FormalContext.", call. = FALSE)
  }
  invisible(NULL)
}

#' @keywords internal
.collect_names <- function(..., universe, what = "name") {
  dots <- list(...)
  if (length(dots) == 0L) {
    stop("Provide at least one ", what, " name.", call. = FALSE)
  }
  if (length(dots) == 1L && inherits(dots[[1L]], "Set")) {
    stop(
      "Pass character names, e.g. intent_of(fc, \"A\", \"B\"). ",
      "For an existing Set, call fc$intent(S) / fc$extent(S) directly.",
      call. = FALSE
    )
  }
  names_vec <- unique(as.character(unlist(dots, use.names = FALSE)))
  names_vec <- names_vec[!is.na(names_vec) & nzchar(names_vec)]
  if (length(names_vec) == 0L) {
    stop("Provide at least one ", what, " name.", call. = FALSE)
  }
  unknown <- setdiff(names_vec, universe)
  if (length(unknown) > 0L) {
    stop(
      "Unknown ", what, "(s): ", paste(unknown, collapse = ", "),
      "\nAvailable: ", paste(universe, collapse = ", "),
      call. = FALSE
    )
  }
  names_vec
}

#' @keywords internal
.set_from_names <- function(universe, elements) {
  S <- Set$new(attributes = universe)
  S$assign(attributes = elements, values = rep(1, length(elements)))
  S
}
