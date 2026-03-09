#' @title
#' Compute bonds between two formal contexts
#'
#' @description
#' A bond between two formal contexts \eqn{K_1} and \eqn{K_2} is a relation
#' between the objects of \eqn{K_1} and the attributes of \eqn{K_2}, such that
#' the relation is closed under the derivation operators of both contexts.
#'
#' @param fc1           (\code{FormalContext}) The first formal context.
#' @param fc2           (\code{FormalContext}) The second formal context.
#' @param method        (character) The method to use. \code{"conexp"} uses
#'   implication-based closed set enumeration on the tensor product.
#'   \code{"mcis"} uses a backtracking algorithm based on pre-computed concepts
#'   (extents of C1 and intents of C2). Default is \code{"conexp"}.
#' @param verbose       (logical) If TRUE, print progress information.
#'
#' @return
#' A \code{BondLattice} object whose intents represent the bonds.
#'
#' @export
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' mat1 <- matrix(sample(0:1, 15, replace = TRUE), nrow = 5, ncol = 3)
#' rownames(mat1) <- paste0("O", 1:5)
#' colnames(mat1) <- paste0("A", 1:3)
#' fc1 <- FormalContext$new(mat1)
#'
#' mat2 <- matrix(sample(0:1, 12, replace = TRUE), nrow = 4, ncol = 3)
#' rownames(mat2) <- paste0("P", 1:4)
#' colnames(mat2) <- paste0("B", 1:3)
#' fc2 <- FormalContext$new(mat2)
#'
#' # Compute bonds returning a lattice
#' bonds_lattice <- bonds(fc1, fc2, method = "conexp")
#' bonds_lattice$print()
#' }
bonds <- function(fc1, fc2, method = c("conexp", "mcis"), verbose = FALSE) {
  method <- match.arg(method)

  if (!inherits(fc1, "FormalContext") || !inherits(fc2, "FormalContext")) {
    stop("Both arguments must be of class FormalContext.", call. = FALSE)
  }

  if (method == "conexp") {
    return(bonds_standard(fc1, fc2, verbose = verbose))
  }

  if (method == "mcis") {
    return(bonds_mcis(fc1, fc2, verbose = verbose))
  }
}

#' @title
#' Verify if a relation is a bond between two formal contexts
#'
#' @description
#' A bond between two formal contexts \eqn{K_1 = (G_1, M_1, I_1)} and \eqn{K_2 = (G_2, M_2, I_2)}
#' is a relation \eqn{R \subseteq G_1 \times M_2} such that every row of \eqn{R} is an intent
#' of \eqn{K_2} and every column of \eqn{R} is an extent of \eqn{K_1}.
#'
#' @param fc1      (\code{FormalContext}) The first formal context.
#' @param fc2      (\code{FormalContext}) The second formal context.
#' @param relation (matrix) A binary matrix or \code{FormalContext} representing the relation between objects of \eqn{fc1} and attributes of \eqn{fc2}.
#'
#' @return \code{TRUE} if the relation is a bond, \code{FALSE} otherwise.
#' @export
is_bond <- function(fc1, fc2, relation) {
  if (!inherits(fc1, "FormalContext") || !inherits(fc2, "FormalContext")) {
    stop("fc1 and fc2 must be FormalContext objects.", call. = FALSE)
  }

  if (inherits(relation, "FormalContext")) {
    mat <- relation$incidence()
  } else {
    mat <- relation
  }

  # Check dimensions
  if (nrow(mat) != length(fc1$objects) || ncol(mat) != length(fc2$attributes)) {
    return(FALSE)
  }

  # Conversion to sparse if needed
  if (!inherits(mat, "dgCMatrix")) {
    mat <- Matrix::Matrix(mat, sparse = TRUE)
  }

  # Check rows are intents of fc2
  for (i in seq_len(nrow(mat))) {
    v_row <- as.numeric(mat[i, ])
    S_row <- Set$new(attributes = fc2$attributes)
    S_row$assign(attributes = fc2$attributes, values = v_row)
    if (!all(abs(as.numeric(fc2$closure(S_row)$get_vector()) - v_row) < 1e-9)) {
       return(FALSE)
    }
  }

  # Check columns are extents of fc1
  for (j in seq_len(ncol(mat))) {
    v_col <- as.numeric(mat[, j])
    S_col <- Set$new(attributes = fc1$objects)
    S_col$assign(attributes = fc1$objects, values = v_col)

    intent_col <- fc1$intent(S_col)
    extent_col <- fc1$extent(intent_col)
    if (!all(abs(as.numeric(extent_col$get_vector()) - v_col) < 1e-9)) {
       return(FALSE)
    }
  }

  return(TRUE)
}

#' @title
#' Compute bonds via standard implication-based method
#'
#' @description
#' Computes bonds using implication merging and closed-set enumeration.
#' Uses a FastBitset-optimized C++ implementation.
#'
#' @param fc1      (\code{FormalContext}) The first formal context.
#' @param fc2      (\code{FormalContext}) The second formal context.
#' @param verbose  (\code{logical}) Print progress info.
#'
#' @importFrom Matrix t
#' @return A \code{BondLattice} object.
bonds_standard <- function(fc1, fc2, verbose = FALSE) {

  mat1 <- methods::as(fc1$incidence(), "matrix")
  mat2 <- methods::as(fc2$incidence(), "matrix")

  res <- bonds_standard_opt_cpp(mat1, mat2, verbose = verbose)
  bonds_intents <- res$intents

  G1 <- fc1$objects
  M2 <- fc2$attributes

  new_attrs <- paste(rep(G1, times = length(M2)), rep(M2, each = length(G1)), sep = "_")
  dimnames(bonds_intents) <- list(new_attrs, paste0("C", seq_len(ncol(bonds_intents))))

  M <- .subset(bonds_intents)
  bonds_extents <- Matrix::t(M)
  dimnames(bonds_extents) <- list(paste0("O", seq_len(nrow(bonds_extents))), colnames(bonds_intents))

  bl <- BondLattice$new(
    extents = bonds_extents,
    intents = bonds_intents,
    objects = rownames(bonds_extents),
    attributes = new_attrs,
    I = matrix(0, nrow = 0, ncol = 0),
    fc1 = fc1,
    fc2 = fc2
  )

  bl$elapsed <- res$elapsed

  return(bl)
}

#' @title
#' Compute bonds via MCIS (backtracking on pre-computed concepts)
#'
#' @description
#' Computes bonds using Algorithm 1 from "Computing bonds between formal contexts",
#' optimized with FastBitset in a unified C++ solver. Pre-computes concepts
#' (extents of C1 and intents of C2) in R, then passes them to C++ for
#' the backtracking search.
#'
#' @param fc1      (\code{FormalContext}) The first formal context.
#' @param fc2      (\code{FormalContext}) The second formal context.
#' @param verbose  (\code{logical}) Print progress info.
#'
#' @importFrom Matrix t
#' @return A \code{BondLattice} object.
bonds_mcis <- function(fc1, fc2, verbose = FALSE) {

  G1 <- fc1$objects
  M2 <- fc2$attributes

  fc1$find_concepts()
  fc2$find_concepts()

  extents1 <- methods::as(fc1$concepts$extents(), "matrix")
  intents2 <- methods::as(fc2$concepts$intents(), "matrix")

  res <- bonds_mcis_cpp(extents1, intents2, verbose = verbose)
  bonds_intents <- res$intents

  new_attrs <- paste(rep(G1, times = length(M2)), rep(M2, each = length(G1)), sep = "_")
  dimnames(bonds_intents) <- list(new_attrs, paste0("C", seq_len(ncol(bonds_intents))))

  M <- .subset(bonds_intents)
  bonds_extents <- Matrix::t(M)
  dimnames(bonds_extents) <- list(paste0("O", seq_len(nrow(bonds_extents))), colnames(bonds_intents))

  bl <- BondLattice$new(
    extents = bonds_extents,
    intents = bonds_intents,
    objects = rownames(bonds_extents),
    attributes = new_attrs,
    I = matrix(0, nrow = 0, ncol = 0),
    fc1 = fc1,
    fc2 = fc2
  )

  bl$elapsed <- res$elapsed

  return(bl)
}
