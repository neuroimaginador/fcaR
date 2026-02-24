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
#' bonds_lattice <- bonds(fc1, fc2)
#' bonds_lattice$print()
#' }
bonds <- function(fc1, fc2) {

  if (!inherits(fc1, "FormalContext") || !inherits(fc2, "FormalContext")) {
    stop("Both arguments must be of class FormalContext.", call. = FALSE)
  }

  G1 <- fc1$objects
  M1 <- fc1$attributes
  G2 <- fc2$objects
  M2 <- fc2$attributes

  # Compute basis of implications for fc1^d and fc2
  fc1d <- fc1$dual()
  fc1d$find_implications()
  imps1 <- fc1d$implications

  fc2$find_implications()
  imps2 <- fc2$implications

  # Attributes for the new ImplicationSet are G1 \times M2
  new_attrs <- paste(rep(G1, times = length(M2)), rep(M2, each = length(G1)), sep = "_")

  LHS <- list()
  RHS <- list()

  # A \times {m} -> B \times {m}
  for (m in M2) {
    for (i in seq_len(imps1$cardinality())) {
      A <- imps1$get_LHS_matrix()[, i]
      B <- imps1$get_RHS_matrix()[, i]

      A_names <- fc1d$attributes[which(A > 0)]
      B_names <- fc1d$attributes[which(B > 0)]

      if (length(A_names) || length(B_names)) {
        LHS <- append(LHS, list(paste(A_names, m, sep = "_")))
        RHS <- append(RHS, list(paste(B_names, m, sep = "_")))
      }
    }
  }

  # {g} \times A -> {g} \times B
  for (g in G1) {
    for (i in seq_len(imps2$cardinality())) {
      A <- imps2$get_LHS_matrix()[, i]
      B <- imps2$get_RHS_matrix()[, i]

      A_names <- fc2$attributes[which(A > 0)]
      B_names <- fc2$attributes[which(B > 0)]

      if (length(A_names) || length(B_names)) {
        LHS <- append(LHS, list(paste(g, A_names, sep = "_")))
        RHS <- append(RHS, list(paste(g, B_names, sep = "_")))
      }
    }
  }

  new_imps <- ImplicationSet$new(attributes = new_attrs)

  for (i in seq_along(LHS)) {
    S_L <- Set$new(attributes = new_attrs)
    if (length(LHS[[i]])) S_L$assign(attributes = LHS[[i]], values = 1)

    S_R <- Set$new(attributes = new_attrs)
    if (length(RHS[[i]])) S_R$assign(attributes = RHS[[i]], values = 1)

    new_imps$add(S_L, S_R)
  }

  # Build Formal Context and return lattice
  fc_bonds <- new_imps$get_standard_context()
  fc_bonds$find_concepts()

  lattice <- fc_bonds$concepts

  bl <- BondLattice$new(
    extents = lattice$extents(),
    intents = lattice$intents(),
    objects = fc_bonds$objects,
    attributes = fc_bonds$attributes,
    I = fc_bonds$I,
    fc1 = fc1,
    fc2 = fc2
  )

  return(bl)
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
    # Use the incidence method in sparse mode
    mat <- relation$incidence(sparse = TRUE)
  } else {
    mat <- relation
  }

  # Check dimensions
  if (nrow(mat) != length(fc1$objects) || ncol(mat) != length(fc2$attributes)) {
    return(FALSE)
  }

  # Conversion to sparse if needed to ensure robustness in extraction calls
  if (!inherits(mat, "dgCMatrix")) {
    mat <- Matrix::Matrix(mat, sparse = TRUE)
  }

  # Check rows are intents of fc2
  # Each row of the relation must be an intent in fc2
  for (i in seq_len(nrow(mat))) {
    v_row <- mat[i, ]
    # Closure (extent then intent) should return the same vector
    if (!all(abs(as.numeric(fc2$closure(v_row)) - as.numeric(v_row)) < 1e-9)) {
       return(FALSE)
    }
  }

  # Check columns are extents of fc1
  # Each column of the relation must be an extent in fc1
  for (j in seq_len(ncol(mat))) {
    v_col <- mat[, j]
    # extent(intent(v_col)) should be v_col
    if (!all(abs(as.numeric(fc1$extent(fc1$intent(v_col))) - as.numeric(v_col)) < 1e-9)) {
       return(FALSE)
    }
  }

  return(TRUE)
}
