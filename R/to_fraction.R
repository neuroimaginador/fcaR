# Write a matrix as rational numbers
#
# @param A       (matrix) The matrix to be written
# @param latex   (boolean) Should the output be in LaTeX?
# @param type    (string) Latex command to produce fractions. Default is \code{frac}. Other possibilities are \code{dfrac} and \code{sfrac} (this one requires the LaTeX package \code{xfrac.sty}).
#
# @return A character string representing the matrix. It can be \code{cat}ed.
#
.to_fraction <- function(A, latex = FALSE, type = "frac") {

  if (is.character(A)) return(A)

  A[abs(A) < 1.e-7] <- 0

  if (!latex) {

    A[] <- fractional::vfractional(A)

    return(A)

  }

  den <- fractional::denominators(A)
  num <- fractional::numerators(A)

  A_chr <- A
  A_chr[] <- as.character(A)
  idx <- which(den != 1)
  idx_neg <- which((num < 0) & (den != 1))
  num <- abs(num)

  A_chr[idx] <- paste0("\\", type, "{", num[idx], "}{", den[idx], "}")
  A_chr[idx_neg] <- paste0("- ", A_chr[idx_neg])

  return(A_chr)

}

.print_binary <- function(A, latex = FALSE) {

  if (is.character(A)) return(A)

  A[abs(A) < 1.e-7] <- 0

  x_chr <- ifelse(latex, "$\\times$", "X")

  A[] <- ifelse(A == 1, x_chr, " ")

  return(A)

}

.print_matrix <- function(M,
                          objects = rownames(M),
                          attributes = colnames(M),
                          width = getOption("width")) {

  M <- cbind(objects, M)
  M <- rbind(c("", attributes), M)

  # Column width
  col_width <- apply(M, 2,
                     function(m) max(stringr::str_length(m))) + 2

  # Columns to show
  ids <- which(cumsum(col_width) < width)

  fmt_row <- rep("%s ", length(ids)) %>%
    stringr::str_flatten()

  M_str <- list()

  for (i in seq(nrow(M))) {

    M_str[[i]] <- sapply(
      ids,
      function(j) {

        stringr::str_pad(M[i, j],
                width = col_width[j],
                side = ifelse(j == 1, "left", "both"))

                })

  }

  write_row <- function(...) do.call(
    function(...) sprintf(fmt = fmt_row, ...),
    as.list(...))

  body <- lapply(M_str, write_row) %>%
    unlist() %>%
    stringr::str_flatten("\n")

  return(list(mat = body,
              att_id = ids))

}
