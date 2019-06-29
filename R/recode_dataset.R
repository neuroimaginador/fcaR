.expand_dataset <- function(I,
                            grades_set = NULL,
                            implications = TRUE) {

  # Formal context properties
  n_attributes <- ncol(I)
  attributes <- colnames(I)

  if (is.null(grades_set)) {

    grades_set <- sort(unique(as.vector(I)))

  }
  grades_set <- grades_set[grades_set > 0]
  n_grades <- length(grades_set)

  # Detect binary columns
  binaries <- .detect_binary_columns(I)
  n_binaries <- length(which(binaries))

  # Expand dataset to binary
  n_new_attributes <- (n_attributes - n_binaries) * n_grades + n_binaries

  new_attributes <- c()

  resI <- matrix(FALSE,
                 nrow = nrow(I),
                 ncol = n_new_attributes)

  my_col <- 1

  for (i in seq(n_attributes)) {

    idx <- which(I[, i] > 0)

    if (binaries[i]) {

      new_attributes <- c(new_attributes, attributes[i])

      resI[idx, my_col] <- TRUE

      my_col <- my_col + 1

    } else {

      new_attributes <- c(new_attributes,
                          paste0(attributes[i], rev(grades_set)))

      j <- match(I[idx, i], grades_set)

      idx <- rep.int(idx, j)
      j <- lapply(j, function(s) n_grades - seq(s) + 1)
      j <- do.call(c, j)

      resI[(j  + my_col - 2) * nrow(I) + idx] <- TRUE

      my_col <- my_col + n_grades

    }

  }

  colnames(resI) <- new_attributes

  if (!implications) return(list(I = resI, binaries = binaries))

  # Generate basic encoding rules
  n_implications <- (n_attributes - n_binaries) * (n_grades - 1)

  LHS <- matrix(FALSE,
                nrow = n_implications,
                ncol = n_new_attributes)
  RHS <- matrix(FALSE,
                nrow = n_implications,
                ncol = n_new_attributes)

  fuzzy_attributes <- setdiff(seq(n_attributes), which(binaries))

  for (g in seq(n_grades - 1)) {

    j_lhs <- g + n_grades * (fuzzy_attributes - 1)
    j_rhs <- j_lhs + 1

    i <- g + (seq_along(fuzzy_attributes) - 1) * (n_grades - 1)

    LHS[(j_lhs - 1) * n_implications + i] <- TRUE
    RHS[(j_rhs - 1) * n_implications + i] <- TRUE

  }

  colnames(LHS) <- new_attributes
  colnames(RHS) <- new_attributes

  return(list(I = resI,
              binaries = binaries,
              implications = list(lhs = LHS, rhs = RHS)))

}

.recode_to_original_grades <- function(I2,
                                       grades_set,
                                       binaries = NULL) {

  n_grades <- length(grades_set[grades_set > 0])

  if (is.null(binaries)) {

    n_attributes <- ncol(I2) / n_grades
    binaries <- rep(FALSE, n_attributes)

  } else {

    n_binaries <- length(which(binaries))

    n_attributes <- (ncol(I2) + n_binaries * (n_grades - 1)) / (n_grades)

  }

  I <- matrix(0, nrow = nrow(I2), ncol = n_attributes)

  grades <- rev(grades_set)

  my_col <- 1

  for (i in seq(n_attributes)) {

    if (binaries[i]) {

      I[, i] <- I2[, my_col]

      my_col <- my_col + 1

    } else {

      tmp <- as.matrix(I2[, my_col - 1 + seq(n_grades)])

      my_col <- my_col + n_grades

      idx <- which(rowSums(tmp) > 0)

      if (length(idx) > 1) {

        g_idx <- apply(tmp[idx, ], 1, which.max)

      } else {

        g_idx <- which.max(tmp[idx, ])

      }

      I[idx, i] <- grades[g_idx]

    }

  }

  return(I)

}
