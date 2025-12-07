# Converts a set of implications into a basis (Canonical Basis or Minimal Cover).
# The algorithm iteratively refines the set of implications to ensure minimality and completeness.
# It consists of two main passes:
# 1. Update the RHS of each implication to be the closure of the union of LHS and RHS relative to the rest of the implications.
# 2. Check for redundancy and remove implications that are entailed by others.
.imp_to_basis <- function(LHS, RHS, attributes) {
  n <- ncol(LHS)
  # Pass 1: "Make it maximal"?
  # For each implication $A \to B$:
  # remove it from the set $\mathcal{L}$.
  # Compute $B' = (A \cup B)^{\mathcal{L} \setminus \{A \to B\}}$.
  # Replace $A \to B$ with $A \to B'$.
  # This ensures that the RHS captures all information derivable from the other rules.
  for (i in seq(n)) {
    A <- Matrix::Matrix(LHS[, 1], sparse = TRUE)
    B <- Matrix::Matrix(RHS[, 1], sparse = TRUE)
    LHS <- Matrix::Matrix(LHS[, -1], sparse = TRUE)
    RHS <- Matrix::Matrix(RHS[, -1], sparse = TRUE)
    AUB <- .union(A, B)
    B <- .compute_closure(AUB, LHS, RHS, attributes, reduce = FALSE)$closure
    LHS <- cbind(LHS, A)
    RHS <- cbind(RHS, B)
  }
  # Pass 2: Remove redundant implications.
  # For each implication $A \to B$:
  # Compute $A^{\mathcal{L} \setminus \{A \to B\}}$.
  # If $A^{\mathcal{L} \setminus \{A \to B\}} = A^{\mathcal{L}}$ (which is $B$ after pass 1, if $A \to B$ is a valid implication where $B$ is the closure?),
  # then this implication is redundant?
  # Actually, the check `if (!(all(A == B)))` implies that if the closure of A (using other rules) reaches B, then the rule A->B is redundant and we don't add it back.
  # If the closure of A without this rule is NOT equal to B (meaning the rule adds new info), we keep it.
  for (i in seq(n)) {
    A <- Matrix::Matrix(LHS[, 1], sparse = TRUE)
    B <- Matrix::Matrix(RHS[, 1], sparse = TRUE)
    LHS <- Matrix::Matrix(LHS[, -1], sparse = TRUE)
    RHS <- Matrix::Matrix(RHS[, -1], sparse = TRUE)
    A <- .compute_closure(A, LHS, RHS, attributes, reduce = FALSE)$closure
    if (!(all(A == B))) {
      LHS <- cbind(LHS, A)
      RHS <- cbind(RHS, B)
    }
  }

  # Return the basis $\Sigma = \{ A \to (B \setminus A) \}$.
  return(list(lhs = LHS, rhs = .difference2(RHS, LHS)))
}

# Completes the RHS of the implications to be closed sets.
# This function applies a similar logic to Pass 1 of .imp_to_basis.
# It ensures that for every $A \to B$, $B$ is replaced by $(A \cup B)^{\mathcal{L}}$.
complete_rhs <- function(LHS, RHS) {
  n <- ncol(LHS)

  for (i in seq(n)) {
    A <- Matrix::Matrix(LHS[, 1], sparse = TRUE) # %>% extract_columns(1)
    B <- Matrix::Matrix(RHS[, 1], sparse = TRUE) # %>% extract_columns(1)

    LHS <- Matrix::Matrix(LHS[, -1], sparse = TRUE) # %>% remove_columns(1)
    RHS <- Matrix::Matrix(RHS[, -1], sparse = TRUE) # %>% remove_columns(1)

    AUB <- .union(A, B)

    # Compute closure with respect to the remaining rules
    B <- .compute_closure(AUB, LHS, RHS, attributes, reduce = FALSE)$closure

    LHS <- cbind(LHS, A)
    RHS <- cbind(RHS, B)
  }

  return(list(lhs = LHS, rhs = .difference2(RHS, LHS)))
}
