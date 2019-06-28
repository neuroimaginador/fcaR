.expand_dataset <- function(I,
                            grades_set = NULL,
                            implications = TRUE) {

  # Formal context properties
  n_attributes <- ncol(I)

  if (is.null(grades_set)) {

    grades_set <- sort(unique(as.vector(I)))

  }
  grades_set <- grades_set[grades_set > 0]
  n_grades <- length(grades_set)

  # Detect binary columns
  binaries <- .detect_binary_columns(I)
  n_binaries <- length(which(binaries))

  # Expand dataset to binary
  resI <- matrix(FALSE,
                 nrow = nrow(I),
                 ncol = n_attributes * n_grades)

  for (i in seq(n_attributes)) {

    idx <- which(I[, i] > 0)

    j <- match(I[idx, i], grades_set)

    idx <- rep.int(idx, j)
    j <- lapply(j, function(s) n_grades - seq(s) + 1)
    j <- do.call(c, j)

    resI[(j  + (i - 1) * n_grades - 1) * nrow(I) + idx] <- TRUE

  }

  if (!implications) return(resI)

  # Generate basic encoding rules
  LHS <- matrix(FALSE,
                nrow = n_attributes * (n_grades - 1),
                ncol = n_attributes * n_grades)
  RHS <- matrix(FALSE,
                nrow = n_attributes * (n_grades - 1),
                ncol = n_attributes * n_grades)

  n_implications <- n_attributes * (n_grades - 1)

  for (g in seq(n_grades - 1)) {

    j_lhs <- g + n_grades * (seq(n_attributes) - 1)
    j_rhs <- j_lhs + 1

    i <- g + (seq(n_attributes) - 1) * (n_grades - 1)

    LHS[(j_lhs - 1) * n_implications + i] <- TRUE
    RHS[(j_rhs - 1) * n_implications + i] <- TRUE

  }


  return(list(I = resI,
              implications = list(lhs = LHS, rhs = RHS)))

}

.recode_to_original_grades <- function(I2, grades_set) {

  n_grades <- length(grades_set)
  n_attributes <- ncol(I2) / n_grades
  I <- matrix(0, nrow = nrow(I2), ncol = n_attributes)

  grades <- rev(grades_set)

  for (i in seq(n_attributes)) {

    tmp <- as.matrix(I2[, (i - 1) * n_grades + seq(n_grades)])

    idx <- which(rowSums(tmp) > 0)

    if (length(idx) > 1) {

      g_idx <- apply(tmp[idx, ], 1, which.max)

    } else {

      g_idx <- which.max(tmp[idx, ])

    }

    I[idx, i] <- grades[g_idx]

  }

  return(I)

}
