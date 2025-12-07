# Computes the closure of a set S with respect to a set of implications (LHS -> RHS).
# Implements the LinClosure algorithm.
# Input: S (set of attributes), LHS, RHS (implications A -> B)
# Output: The closure $S^{\mathcal{L}}$ such that $S \subseteq S^{\mathcal{L}}$ and $S^{\mathcal{L}}$ is closed under implications.
# The algorithm works by iteratively adding B to S whenever A is a subset of S, for all implications A -> B.
# This linear time closure algorithm is crucial for minimal basis computations.
.compute_closure <- function(
  S,
  LHS,
  RHS,
  attributes,
  reduce = FALSE,
  verbose = FALSE,
  is_direct = FALSE
) {
  if (is.null(LHS) || (ncol(LHS) == 0)) {
    return(list(closure = S, implications = list(lhs = LHS, rhs = RHS)))
  }

  # Which are the rules applicable to the set S?
  # We look for indices i such that LHS[, i] is a subset of S.
  S_subsets <- .subset(LHS, S)

  idx_subsets <- S_subsets@i + 1

  do_not_use <- rep(FALSE, ncol(LHS))

  passes <- 0

  # While there are applicable rules, apply them!
  # Loop condition: While $\exists (A \to B) \in \Sigma$ such that $A \subseteq S$ (and potentially $B \not\subseteq S$):
  # Update: $S = S \cup B$
  # We continue until no new implications can be fired.
  while (length(idx_subsets) > 0) {
    passes <- passes + 1
    if (verbose) {
      cat("Pass #", passes, "\n")
    }

    if (length(idx_subsets) == 1) {
      A <- Matrix::Matrix(RHS[, idx_subsets], sparse = TRUE)
    } else {
      A <- RHS[, idx_subsets]
    }

    # Add the RHS of triggered implications to S
    # S_{new} = S_{old} \cup \bigcup_{i \in idx} RHS_i
    S <- .multiunion(add_col(A, S))

    # Mark these implications as used so we don't check them again if not needed
    # (Though in standard LinClosure we might check everything, here we optimize)
    do_not_use[idx_subsets] <- TRUE

    if (reduce) {
      # If reduce is TRUE, we apply simplification logic to the implications set
      # based on the current closure to reduce the size of the basis being built.
      L <- .simplification_logic(S = S, LHS = LHS, RHS = RHS)

      LHS <- L$lhs
      RHS <- L$rhs

      for (rem in L$idx_removed) {
        do_not_use <- do_not_use[-rem]
      }
    }

    if (is.null(LHS) || (ncol(LHS) == 0)) {
      return(list(closure = S, implications = list(lhs = LHS, rhs = RHS)))
    }

    if (!is_direct) {
      # Re-evaluate subsets for the next pass since S has grown.
      S_subsets <- .subset(LHS, S)

      idx_subsets <- S_subsets@i + 1
      # Avoid using implications that were already fully processed/removed
      idx_subsets <- setdiff(idx_subsets, which(do_not_use))

      if (verbose) {
        print(idx_subsets)
        print(Set$new(attributes = attributes, M = S))
        cat("\n")
      }
    } else {
      idx_subsets <- c()
    }
  }

  if (reduce) {
    return(list(closure = S, implications = .simplification_logic(S, LHS, RHS)))
  } else {
    return(list(closure = S, implications = list(LHS, RHS)))
  }
}

# Mathematical simplification logic for implications.
# Applies equivalence rules (like removing redundant attributes from LHS if they are in RHS, etc.)
# Specifically Equivalence II and III from some literature (e.g. Maier).
.simplification_logic <- function(S, LHS, RHS) {
  # Equivalence II (Simpification by Entailment/Satisfaction)
  # If $B \subseteq S$, then the implication $A \to B$ is satisfied by $S$ (if we consider S as a model)
  # or if we are just reducing the basis relative to what we already know (S).
  subsets <- .subset(RHS, S)
  idx_subsets <- subsets@i + 1

  idx_removed <- list()

  if (length(idx_subsets) > 0) {
    idx_removed[[1]] <- idx_subsets

    LHS <- Matrix::Matrix(LHS[, -idx_subsets], sparse = TRUE)
    RHS <- Matrix::Matrix(RHS[, -idx_subsets], sparse = TRUE)
  }

  if (ncol(LHS) == 0) {
    return(list(lhs = NULL, rhs = NULL))
  }

  # Equivalence III (Generalization / Composition)
  # We look for overlap between $S$ and $A \cup B$.
  # Let $C = LHS, D = RHS$.
  C <- LHS
  D <- RHS

  CD <- .union(LHS, RHS)

  intersections <- .intersection(x = S, y = CD)
  idx_not_empty <- Matrix::which(Matrix::colSums(intersections) > 0)

  if (length(idx_not_empty) > 0) {
    if (length(idx_not_empty) == 1) {
      Cidx <- .extract_column(C, idx_not_empty)
      Didx <- .extract_column(D, idx_not_empty)
    } else {
      Cidx <- C[, idx_not_empty]
      Didx <- D[, idx_not_empty]
    }

    # C_B = C \setminus S
    C_B <- set_difference_single(
      Cidx@i,
      Cidx@p,
      Cidx@x,
      S@i,
      S@p,
      S@x,
      nrow(Cidx)
    )

    # D_B = D \setminus S
    D_B <- set_difference_single(
      Didx@i,
      Didx@p,
      Didx@x,
      S@i,
      S@p,
      S@x,
      nrow(Didx)
    )

    # If D \setminus S is empty, the rule is trivialised to possibly A \to \emptyset (redundant)
    idx_zeros <- Matrix::which(Matrix::colSums(D_B) == 0)

    if (length(idx_zeros) > 0) {
      C_B <- Matrix::Matrix(C_B[, -idx_zeros], sparse = TRUE)
      D_B <- Matrix::Matrix(D_B[, -idx_zeros], sparse = TRUE)
    }

    # Reconstruct LHS/RHS with simplified parts
    LHS <- cbind(C_B, Matrix::Matrix(C[, -idx_not_empty], sparse = TRUE))
    RHS <- cbind(D_B, Matrix::Matrix(D[, -idx_not_empty], sparse = TRUE))
  }

  # Further simplification steps using generalization and composition rules
  L <- .generalization(LHS, RHS)
  L <- .composition(L$lhs, L$rhs)
  LHS <- L$lhs
  RHS <- L$rhs

  return(list(lhs = LHS, rhs = RHS, idx_removed = idx_removed))
}
