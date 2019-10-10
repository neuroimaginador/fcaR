#' @import digest
compute_row_hash <- function(dgCMat, include.all.zero.vectors = FALSE) {

  J <- rep(1:ncol(dgCMat), diff(dgCMat@p))

  I <- dgCMat@i + 1

  x <- dgCMat@x

  names(x) <- J
  if (include.all.zero.vectors) {
    RowLst <- split(x, factor(I, levels = 1:nrow(dgCMat)))
  } else {
    RowLst <- split(x, I)  ## will do `factor(I)` internally in `split`
  }

  result <- sapply(RowLst, digest::digest)

  return(result)

}

#' @import digest
compute_col_hash <- function(dgCMat, include.all.zero.vectors = FALSE) {

  J <- rep(1:ncol(dgCMat), diff(dgCMat@p))

  I <- dgCMat@i + 1

  x <- dgCMat@x

  names(x) <- I
  if (include.all.zero.vectors) {
    RowLst <- split(x, factor(J, levels = 1:nrow(dgCMat)))
  } else {
    RowLst <- split(x, J)  ## will do `factor(I)` internally in `split`
  }

  result <- sapply(RowLst, digest::digest)

  return(result)

}

split_cols <- function(M) {

  J <- rep(1:ncol(M), diff(M@p))

  I <- M@i + 1

  x <- M@x

  names(x) <- I
  RowLst <- split(x, J)

}

split_cols <- function(M) {

  J <- rep(1:ncol(M), diff(M@p))

  I <- M@i + 1

  x <- M@x

  names(x) <- I
  RowLst <- split(x, factor(J, levels = 1:nrow(M)))

}
