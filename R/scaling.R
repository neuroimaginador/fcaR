nominal_scaling <- function(V, col_name,
                            values = sort(unique(V))) {

  # Scale (context)
  vals <- sort(unique(V))
  scale_matrix <- outer(vals, values, `==`)
  rownames(scale_matrix) <- as.character(vals)
  colnames(scale_matrix) <- paste0(col_name, " = ",
                                   as.character(values))
  scale_matrix[] <- as.numeric(scale_matrix)

  return(scale_matrix)

}

ordinal_scaling <- function(V, col_name,
                            values = sort(unique(V)),
                            comparison = `<=`) {

  # browser()

  if (comparison(0, 1)) {

    comp_str <- " <= "

  } else {

    comp_str <- " >= "

  }

  # Scale (context)
  vals <- sort(unique(V))

  if (is.character(V) | is.factor(V)) {

    vals <- as.character(vals)

    vals <- sort(unique(c(vals, values)))
    values <- forcats::as_factor(values)
    vals <- factor(vals, levels = levels(values)) %>% sort()

    if (anyNA(vals)) {

      stop("If the attribute is given by a string, values must order all possible strings.",
           call. = FALSE)

    }

  }

  scale_matrix <- outer(as.numeric(vals),
                        as.numeric(values), comparison)
  rownames(scale_matrix) <- as.character(vals)
  colnames(scale_matrix) <- paste0(col_name, comp_str, values)
  scale_matrix[] <- as.numeric(scale_matrix)

  return(scale_matrix)

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

  # Scale (context)
  vals <- sort(unique(V))

  if (is.character(V) | is.factor(V)) {

    vals <- as.character(vals)

    vals <- sort(unique(c(V, values)))

    values <- forcats::as_factor(values)
    vals <- factor(vals, levels = levels(values)) %>% sort()

    if (anyNA(vals)) {

      stop("If the attribute is given by a string, values must order all possible strings.",
           call. = FALSE)

    }

  }

  scale_matrix <- cbind(outer(as.numeric(vals), as.numeric(values), comparison),
                        !outer(as.numeric(vals), as.numeric(values), FUN = comparison) |
                          outer(as.numeric(vals), as.numeric(values), FUN = `==`))
  rownames(scale_matrix) <- as.character(vals)
  colnames(scale_matrix) <- c(paste0(col_name, comp1_str, values),
                              paste0(col_name, comp2_str, values))
  scale_matrix[] <- as.numeric(scale_matrix)

  return(scale_matrix)

}

biordinal_scaling <- function(V, col_name,
                              values_le = sort((unique(V))),
                              values_ge = values_le,
                              comparison = `<=`) {

  if (comparison(0, 1)) {

    comp1_str <- " <= "
    comp2_str <- " >= "

  } else {

    comp1_str <- " >= "
    comp2_str <- " <= "

  }

  # Scale (context)
  vals <- sort(unique(V))

  if (is.character(V) | is.factor(V)) {

    vals <- as.character(vals)

    all_values <- c(values_le, values_ge)
    all_values <- forcats::as_factor(all_values)
    vals <- sort(unique(c(vals, values_le, values_ge)))
    values_le <- factor(values_le, levels = levels(all_values))
    values_ge <- factor(values_ge, levels = levels(all_values))
    vals <- factor(vals, levels = levels(all_values)) %>%
      sort()

    if (anyNA(vals)) {

      stop("If the attribute is given by a string, values must order all possible strings.",
           call. = FALSE)

    }

  }

  scale_matrix <- cbind(outer(as.numeric(vals), as.numeric(values_le), comparison),
                        !outer(as.numeric(vals), as.numeric(values_ge), FUN = comparison) |
                          outer(as.numeric(vals), as.numeric(values_ge), FUN = `==`))
  rownames(scale_matrix) <- as.character(vals)
  colnames(scale_matrix) <- c(paste0(col_name, comp1_str, values_le),
                              paste0(col_name, comp2_str, values_ge))
  scale_matrix[] <- as.numeric(scale_matrix)

  return(scale_matrix)

}

interval_scaling <- function(V, col_name, interval_names,
                             values = sort(unique(V))) {

  if (missing(interval_names)) {

    interval_names <- paste0("(", values[seq(length(values) - 1)], ", ",
                             values[-1], "]")

  }

  # Scale (context)
  vals <- sort(unique(V))
  A <- outer(vals, values[-1], FUN = `<=`)
  B <- outer(vals, values[seq(length(values) - 1)], FUN = `>`)

  scale_matrix <- A & B
  colnames(scale_matrix) <- paste0(col_name, " is ", interval_names)
  rownames(scale_matrix) <- vals
  scale_matrix[] <- as.numeric(scale_matrix)

  return(scale_matrix)

}

implication_scaling <- function(V, col_name,
                             values = sort(unique(c(0, V, 1)))) {

  I <- function(a, b) ifelse(a >= b, 1, a)

  # Scale (context)
  vals <- sort(unique(c(0, V, 1)))
  scale_matrix <- outer(vals, values, I)
  rownames(scale_matrix) <- as.character(vals)
  colnames(scale_matrix) <- paste0("(", col_name, ", ",
                                   as.character(values), ")")
  scale_matrix[] <- as.numeric(scale_matrix)

  return(scale_matrix)

}

scale_context <- function(I, column, type, bg, ...) {

  idx <- which(colnames(I) == column)
  prev <- seq_len(idx - 1)
  post <- seq(idx, ncol(I))[-1]
  V <- I[, idx]
  fun <- scalingRegistry$get_entry(type)$fun
  args <- as.list(...)
  args$col_name <- column
  args$V <- as.matrix(V)
  # scale_matrix <- fun(as.matrix(V), col_name = column, ...)
  id_nok <- sapply(args, purrr::is_empty)
  args[id_nok] <- NULL
  scale_matrix <- do.call(fun, args)

  # The scale
  scale <- FormalContext$new(scale_matrix)

  # Derived context
  M <- apply_scale(as.matrix(V), scale_matrix)

  res <- cbind.data.frame(I[, prev], M, I[, post])
  colnames(res) <- c(colnames(I)[prev],
                     colnames(M),
                     colnames(I)[post])

  if (bg) {

    # Scale background implications
    scale$find_implications(save_concepts = FALSE)
    imps <- scale$implications$clone()

  } else {

    imps <- ImplicationSet$new(
      attributes = scale$attributes,
      I = scale$I)

  }

  return(list(derived = res,
              scale = scale,
              bg_implications = imps))

}

apply_scale <- function(V, scale_matrix) {

  n <- ncol(scale_matrix)
  m <- nrow(V)
  if (is.null(m)) m <- length(V)
  res <- matrix(0, ncol = n, nrow = m)
  attributes <- rownames(scale_matrix)
  id <- match(V, attributes)
  id1 <- which(!is.na(id))
  res[id1, ] <- scale_matrix[id[id1], ]
  colnames(res) <- colnames(scale_matrix)

  return(res)

}
