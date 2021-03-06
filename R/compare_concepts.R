#' Partial Order in Sets and Concepts
#'
#' @param C1 A \code{SparseSet} or \code{SparseConcept}
#' @param C2 A \code{SparseSet} or \code{SparseConcept}
#'
#' @details
#' Both \code{C1} and \code{C2} must be of the same class.
#'
#' @return
#' Returns \code{TRUE} if concept \code{C1} is subconcept of \code{C2} or if set \code{C1} is subset of \code{C2}.
#'
#' @examples
#' # Build two sparse sets
#' S <- SparseSet$new(attributes = c("A", "B", "C"))
#' S$assign(A = 1)
#' T <- SparseSet$new(attributes = c("A", "B", "C"))
#' T$assign(A = 1, B = 1)
#'
#' # Test whether S is subset of T
#' S %<=% T
#'
#' @export
`%<=%` <- function(C1, C2) {

  if (inherits(C1, "SparseConcept") &
      inherits(C2, "SparseConcept")) {

    return(all(to_matrix.SpM(C1$get_extent()$get_vector()) <= to_matrix.SpM(C2$get_extent()$get_vector())))

  }

  if (inherits(C1, "SparseSet") |
      inherits(C2, "SparseSet")) {

    return(all(to_matrix.SpM(C1$get_vector()) <= to_matrix.SpM(C2$get_vector())))

  }

  stop("Only implemented for SparseConcepts and SparseSets.\n",
       call. = FALSE)

}

#' Equality in Sets and Concepts
#'
#' @param C1 A \code{SparseSet} or \code{SparseConcept}
#' @param C2 A \code{SparseSet} or \code{SparseConcept}
#'
#' @details
#' Both \code{C1} and \code{C2} must be of the same class.
#'
#' @return
#' Returns \code{TRUE} if \code{C1} is equal to \code{C2}.
#'
#' @examples
#' # Build two sparse sets
#' S <- SparseSet$new(attributes = c("A", "B", "C"))
#' S$assign(A = 1)
#' T <- SparseSet$new(attributes = c("A", "B", "C"))
#' T$assign(A = 1)
#'
#' # Test whether S and T are equal
#' S %==% T
#'
#' @export
`%==%` <- function(C1, C2) {

  # Equality of sets/concepts
  if (inherits(C1, "SparseConcept") &
      inherits(C2, "SparseConcept")) {

    return(all(to_matrix.SpM(C1$get_extent()$get_vector()) == to_matrix.SpM(C2$get_extent()$get_vector())))

  }

  if (inherits(C1, "SparseSet") |
      inherits(C2, "SparseSet")) {

    return(all(to_matrix.SpM(C1$get_vector()) == to_matrix.SpM(C2$get_vector())))

  }

  stop("Only implemented for SparseConcepts and SparseSets.\n",
       call. = FALSE)

}

#' Difference in SparseSets
#'
#' @param S1 A \code{SparseSet}
#' @param S2 A \code{SparseSet}
#'
#' @details
#' Both \code{S1} and \code{S2} must be SparseSets.
#'
#' @return
#' Returns the difference \code{S1 - S2}.
#'
#' @examples
#' # Build two sparse sets
#' S <- SparseSet$new(attributes = c("A", "B", "C"))
#' S$assign(A = 1, B = 1)
#' T <- SparseSet$new(attributes = c("A", "B", "C"))
#' T$assign(A = 1)
#'
#' # Difference
#' S %-% T
#'
#' @export
`%-%` <- function(S1, S2) {

  # Fuzzy set difference
  if (inherits(S1, "SparseSet") &
      inherits(S2, "SparseSet")) {

    A <- S1$get_vector()
    B <- S2$get_vector()
    my_diff <- differenceSpM(A, B)

    S <- SparseSet$new(attributes = S1$get_attributes(),
                       M = my_diff)

    return(S)

  }

  stop("Only implemented for SparseSets.\n",
       call. = FALSE)

}
