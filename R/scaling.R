nominal_scaling <- function(V, col_name,
                            values = sort(unique(V))) {

  # Derived context
  A <- outer(V, values, `==`)

  colnames(A) <- paste0(col_name, " = ",
                        as.character(values))

  A[] <- as.numeric(A)

  # Scale (context)
  vals <- sort(unique(V))
  scale_matrix <- outer(vals, values, `==`)
  rownames(scale_matrix) <- as.character(vals)
  colnames(scale_matrix) <- colnames(A)
  scale <- FormalContext$new(scale_matrix)

  return(list(derived = A,
              scale = scale))

}

ordinal_scaling <- function(V, col_name,
                            values = sort(unique(V)),
                            comparison = `<=`) {

  if (comparison(0, 1)) {

    comp_str <- " <= "

  } else {

    comp_str <- " >= "

  }

  # Derived context
  A <- outer(V, values, FUN = comparison)
  colnames(A) <- paste0(col_name, comp_str, values)

  A[] <- as.numeric(A)

  # Scale (context)
  vals <- sort(unique(V))
  scale_matrix <- outer(vals, values, comparison)
  rownames(scale_matrix) <- as.character(vals)
  colnames(scale_matrix) <- colnames(A)
  scale <- FormalContext$new(scale_matrix)

  return(list(derived = A,
              scale = scale))
}

interordinal_scaling <- function(V, col_name,
                                 values = sort(unique(V)),
                                 comparison = `<=`) {

  if (comparison(0, 1)) {

    comp1_str <- " <= "
    comp2_str <- " >= "

  } else {

    comp1_str <- " >= "
    comp2_str <- " <= "

  }

  # Derived context
  A <- outer(V, values, FUN = comparison)
  colnames(A) <- paste0(col_name, comp1_str, values)
  B <- !outer(V, values, FUN = comparison) | outer(V, values, FUN = `==`)
  colnames(B) <- paste0(col_name, comp2_str, values)

  A <- cbind(A, B)
  A[] <- as.numeric(A)

  # Scale (context)
  vals <- sort(unique(V))
  scale_matrix <- cbind(outer(vals, values, comparison),
                        !outer(vals, values, FUN = comparison) |
                          outer(vals, values, FUN = `==`))
  rownames(scale_matrix) <- as.character(vals)
  colnames(scale_matrix) <- colnames(A)
  scale <- FormalContext$new(scale_matrix)

  return(list(derived = A,
              scale = scale))
}

biordinal_scaling <- function(V, col_name,
                              values_neg = sort((unique(V))),
                              values_pos = values_neg,
                              comparison = `<=`) {

  if (comparison(0, 1)) {

    comp1_str <- " <= "
    comp2_str <- " >= "

  } else {

    comp1_str <- " >= "
    comp2_str <- " <= "

  }

  # Derived context
  A <- outer(V, values_neg, FUN = comparison)
  colnames(A) <- paste0(col_name, comp1_str, values)
  B <- !outer(V, values_pos, FUN = comparison) |
    outer(V, values_pos, FUN = `==`)
  colnames(B) <- paste0(col_name, comp2_str, values)

  A <- cbind(A, B)
  A[] <- as.numeric(A)

  # Scale (context)
  vals <- sort(unique(V))
  scale_matrix <- cbind(outer(vals, values_neg, comparison),
                        !outer(vals, values_pos, FUN = comparison) |
                          outer(vals, values_pos, FUN = `==`))
  rownames(scale_matrix) <- as.character(vals)
  colnames(scale_matrix) <- colnames(A)
  scale <- FormalContext$new(scale_matrix)

  return(list(derived = A,
              scale = scale))
}

interval_scaling <- function(V, col_name, interval_names,
                             values = sort(unique(V))) {

  if (missing(interval_names)) {

    interval_names <- paste0("(", values[seq(length(values) - 1)], ", ",
                             values[-1], "]")

  }

  # Derived context
  A <- outer(V, values[-1], FUN = `<=`)
  B <- outer(V, values[seq(length(values) - 1)], FUN = `>`)

  I <- A & B
  colnames(I) <- paste0(col_name, " is ", interval_names)

  I[] <- as.numeric(I)

  # Scale (context)
  vals <- sort(unique(V))
  A <- outer(vals, values[-1], FUN = `<=`)
  B <- outer(vals, values[seq(length(values) - 1)], FUN = `>`)

  scale_matrix <- A & B
  colnames(scale_matrix) <- paste0(col_name, " is ", interval_names)
  scale_matrix[] <- as.numeric(scale_matrix)
  scale <- FormalContext$new(scale_matrix)

  return(list(derived = I,
              scale = scale))
}

scale_context <- function(I, column, type, ...) {

  idx <- which(colnames(I) == column)
  prev <- seq_len(idx - 1)
  post <- seq(idx, ncol(I))[-1]
  V <- I[, idx]
  fun <- scalingRegistry$get_entry(type)$fun
  scaled <- fun(V, col_name = column, ...)
  M <- scaled$derived

  res <- cbind(I[, prev], M, I[, post])
  colnames(res) <- c(colnames(I)[prev],
                     colnames(M),
                     colnames(I)[post])

  # Scale background implications
  scaled$scale$find_implications()
  imps <- scaled$scale$implications$clone()

  return(list(derived = res,
              scale = scaled$scale,
              bg_implications = imps))

}
