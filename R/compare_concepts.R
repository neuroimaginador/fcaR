#' Partial Order in Sets and Concepts
#'
#' @param C1 A \code{Set} or \code{Concept}
#' @param C2 A \code{Set} or \code{Concept}
#'
#' @details
#' Both \code{C1} and \code{C2} must be of the same class.
#'
#' @return
#' Returns \code{TRUE} if concept \code{C1} is subconcept of \code{C2} or if set \code{C1} is subset of \code{C2}.
#'
#' @examples
#' # Build two sparse sets
#' S <- Set$new(attributes = c("A", "B", "C"))
#' S$assign(A = 1)
#' T <- Set$new(attributes = c("A", "B", "C"))
#' T$assign(A = 1, B = 1)
#'
#' # Test whether S is subset of T
#' S %<=% T
#'
#' @export
`%<=%` <- function(C1, C2) {

  if (inherits(C1, "Concept") &&
      inherits(C2, "Concept")) {

    return(all(C1$get_extent()$get_vector() <= C2$get_extent()$get_vector()))

  }

  if (inherits(C1, "Set") ||
      inherits(C2, "Set")) {

    return(all(C1$get_vector() <= C2$get_vector()))

  }

  stop("Only implemented for Concepts and Sets.\n",
       call. = FALSE)

}

#' Equality in Sets and Concepts
#'
#' @param C1 A \code{Set} or \code{Concept}
#' @param C2 A \code{Set} or \code{Concept}
#'
#' @details
#' Both \code{C1} and \code{C2} must be of the same class.
#'
#' @return
#' Returns \code{TRUE} if \code{C1} is equal to \code{C2}.
#'
#' @examples
#' # Build two sparse sets
#' S <- Set$new(attributes = c("A", "B", "C"))
#' S$assign(A = 1)
#' T <- Set$new(attributes = c("A", "B", "C"))
#' T$assign(A = 1)
#'
#' # Test whether S and T are equal
#' S %==% T
#'
#' @export
`%==%` <- function(C1, C2) {

  # Equality of sets/concepts
  if (inherits(C1, "Concept") &&
      inherits(C2, "Concept")) {

    return(all(C1$get_extent()$get_vector() == C2$get_extent()$get_vector()))

  }

  if (inherits(C1, "Set") ||
      inherits(C2, "Set")) {

    return(all(C1$get_vector() == C2$get_vector()))

  }

  stop("Only implemented for Concepts and Sets.\n",
       call. = FALSE)

}

#' Difference in Sets
#'
#' @param S1 A \code{Set}
#' @param S2 A \code{Set}
#'
#' @details
#' Both \code{S1} and \code{S2} must be Sets.
#'
#' @return
#' Returns the difference \code{S1 - S2}.
#'
#' @examples
#' # Build two sparse sets
#' S <- Set$new(attributes = c("A", "B", "C"))
#' S$assign(A = 1, B = 1)
#' T <- Set$new(attributes = c("A", "B", "C"))
#' T$assign(A = 1)
#'
#' # Difference
#' S %-% T
#'
#' @export
`%-%` <- function(S1, S2) {

  # Fuzzy set difference
  if (inherits(S1, "Set") &&
      inherits(S2, "Set")) {

    A <- S1$get_vector()
    B <- S2$get_vector()
    my_diff <- .difference2(A, B) |>
      Matrix::as.matrix() |> as.vector()
    names(my_diff) <- S1$get_attributes()

    S <- as_Set(my_diff)

    return(S)

  }

  stop("Only implemented for Sets.\n",
       call. = FALSE)

}

#' Intersection (Logical AND) of Fuzzy Sets
#'
#' @param S1 A \code{Set}
#' @param S2 A \code{Set}
#'
#' @details
#' Both \code{S1} and \code{S2} must be Sets.
#'
#' @return
#' Returns the intersection of \code{S1} and \code{S2}.
#'
#' @examples
#' # Build two sparse sets
#' S <- Set$new(attributes = c("A", "B", "C"))
#' S$assign(A = 1, B = 1)
#' T <- Set$new(attributes = c("A", "B", "C"))
#' T$assign(A = 1, C = 1)
#'
#' # Intersection
#' S %&% T
#'
#' @export
`%&%` <- function(S1, S2) {

  # Fuzzy set difference
  if (inherits(S1, "Set") &&
      inherits(S2, "Set")) {

    att <- S1$get_attributes()

    A <- S1$get_vector()
    B <- S2$get_vector()
    my_diff <- set_intersection_single(A@i, A@p, A@x,
                                       B@i, B@p, B@x,
                                       nrow(A))
    S <- Set$new(attributes = att, M = my_diff)

    return(S)

  }

  stop("Only implemented for Sets.\n",
       call. = FALSE)

}

#' Union (Logical OR) of Fuzzy Sets
#'
#' @param S1 A \code{Set}
#' @param S2 A \code{Set}
#'
#' @details
#' Both \code{S1} and \code{S2} must be Sets.
#'
#' @return
#' Returns the union of \code{S1} and \code{S2}.
#'
#' @examples
#' # Build two sparse sets
#' S <- Set$new(attributes = c("A", "B", "C"))
#' S$assign(A = 1, B = 1)
#' T <- Set$new(attributes = c("A", "B", "C"))
#' T$assign(C = 1)
#'
#' # Union
#' S %|% T
#'
#' @name %or%
#'
#' @export
`%|%` <- function(S1, S2) {

  # Fuzzy set difference
  if (inherits(S1, "Set") &&
      inherits(S2, "Set")) {

    att <- S1$get_attributes()

    A <- S1$get_vector()
    B <- S2$get_vector()
    my_diff <- .union(A, B)
    S <- Set$new(attributes = att, M = my_diff)

    return(S)

  }

  stop("Only implemented for Sets.\n",
       call. = FALSE)

}

