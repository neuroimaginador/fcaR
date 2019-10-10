# A -> B |= A -> B\A
.reduction <- function(imp) {

  A <- imp$get_lhs()
  B <- imp$get_rhs()

  new_B <- B - A

  if (gset_is_empty(new_B)) {

    imp <- NULL

  } else {

    imp <- implication$new(lhs = A, rhs = new_B)

  }

  return(list(changed = !(new_B == B),
              implication = imp))

}

# A -> B y A -> D |= A -> BD
.composition <- function(imp1, imp2) {

  A <- imp1$get_lhs()
  B <- imp1$get_rhs()

  C <- imp2$get_lhs()
  D <- imp2$get_rhs()

  changed <- FALSE
  if (A == C) {

    new_B <- gset_union(B, D)

    imp1 <- implication$new(lhs = A, rhs = new_B)
    imp2 <- NULL

    changed <- TRUE

  }

  return(list(changed = changed,
              imp1 = imp1,
              imp2 = imp2))

}

# A -> B, C -> D and A subset of C |= A -> B and C\B -> D\B
.simplification <- function(imp1, imp2) {

  A <- imp1$get_lhs()
  B <- imp1$get_rhs()

  C <- imp2$get_lhs()
  D <- imp2$get_rhs()

  changed <- FALSE

  if (A <= C) {

    C_B <- C - B
    D_B <- D - B

    if (gset_is_empty(C_B) || gset_is_empty(D_B)) {

      imp2 <- NULL
      changed <- TRUE

    } else {

      if (!(C_B == C) || !(D_B == D)) {

        imp2 <- implication$new(lhs = C_B, rhs = D_B)
        changed <- TRUE

      }

      if (C_B == A) {

        L <- .composition(imp1, imp2)

        changed <- L$changed
        imp1 <- L$imp1
        imp2 <- L$imp2

      }

    }

  }

  return(list(changed = changed,
              imp1 = imp1,
              imp2 = imp2))

}

# A -> B, C -> D, with A <= CD |= C -> D\B
.r_simplification <- function(imp1, imp2) {

  A <- imp1$get_lhs()
  B <- imp1$get_rhs()

  C <- imp2$get_lhs()
  D <- imp2$get_rhs()

  changed <- FALSE

  CD <- gset_union(C, D)

  if (A <= CD) {

    D_B <- D - B

    if (gset_is_empty(D_B)) {

      imp2 <- NULL
      changed <- TRUE

    } else {

      if (!(D_B == D)) {

        changed <- TRUE
        imp2 <- implication$new(lhs = C,
                                rhs = D_B)

      }

    }

  }

  return(list(changed = changed,
              imp1 = imp1,
              imp2 = imp2))

}

### Simplification (inference) rule
.simplification_rule <- function(imp1, imp2) {

  A <- imp1$get_lhs()
  B <- imp1$get_rhs()

  C <- imp2$get_lhs()
  D <- imp2$get_rhs()

  C_B <- C - B

  new_lhs <- gset_union(A, C_B)
  imp <- implication$new(lhs = new_lhs,
                         rhs = D)

  return(imp)

}


### Generalized transitivity
# Do we remove imp1?
.generalized_transitivity <- function(imp1, imp2, imp3) {

  V <- imp1$get_lhs()
  W <- imp1$get_rhs()

  X <- imp2$get_lhs()
  Y <- imp2$get_rhs()

  XY <- gset_union(X, Y)

  Z <- imp3$get_lhs()
  U <- imp3$get_rhs()

  changed <- FALSE

  if (Z <= XY) {

    UV <- gset_union(U, V)

    if ((X <= V) && (W <= UV)) {

      changed <- TRUE

    }

  }

  return(changed)

}
