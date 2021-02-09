nominal_scaling <- function(V, col_name) {

  if (is.factor(V)) {

    cl_num <- as.numeric(V)

  } else {

    V <- as.factor(V)
    cl_num <- as.numeric(V)

  }
  A <- matrix(0, nrow = length(V), ncol = max(cl_num))
  ii <- seq_along(V)
  A[(cl_num - 1) * length(cl_num) + ii] <- 1

  colnames(A) <- paste0(col_name, " = ",
                        as.character(levels(V)))

  return(A)

}

ordinal_scaling <- function(V, col_name,
                            values = sort(unique(V)),
                            comparison = `<=`) {

  A <- outer(V, values, FUN = comparison)
  colnames(A) <- paste0(col_name, " <= ", values)

  return(A)

}

interordinal_scaling <- function(V, col_name,
                                 values = sort(unique(V)),
                                 comparison = `<=`) {

  A <- outer(V, values, FUN = comparison)
  colnames(A) <- paste0(col_name, " <= ", values)
  B <- !outer(V, values, FUN = comparison) | outer(V, values, FUN = `==`)
  colnames(B) <- paste0(col_name, " >= ", values)

  return(cbind(A, B))

}

biordinal_scaling <- function(V, col_name,
                              values_neg = sort((unique(V))),
                              values_pos = values_neg,
                              comparison = `<=`) {

  A <- outer(V, values_neg, FUN = comparison)
  colnames(A) <- paste0(col_name, " <= ", values)
  B <- !outer(V, values_pos, FUN = comparison)
  colnames(B) <- paste0(col_name, " >= ", values)

  return(cbind(A, B))

}

interval_scaling <- function(V, col_name, interval_names,
                             values = sort(unique(V))) {

  if (missing(interval_names)) {

    interval_names <- paste0("(", values[seq(length(values) - 1)], ", ",
                             values[-1], "]")

  }

  A <- outer(V, values[-1], FUN = `<=`)
  B <- outer(V, values[seq(length(values) - 1)], FUN = `>`)

  I <- A & B
  colnames(I) <- paste0(col_name, " is ", interval_names)

  return(I)

}

scale_context <- function(I, column, type, ...) {

  idx <- which(colnames(I) == column)
  prev <- seq_len(idx - 1)
  post <- seq(idx, ncol(I))[-1]
  V <- I[, idx]
  fun <- scalingRegistry$get_entry(type)$fun
  A <- fun(V, col_name = column, ...)

  res <- cbind(I[, prev], A, I[, post])
  colnames(res) <- c(colnames(I)[prev],
                     colnames(A),
                     colnames(I)[post])

  return(res)

}
