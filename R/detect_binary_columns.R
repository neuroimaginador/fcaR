.detect_binary_columns <- function(I) {

  L <- lapply(seq(ncol(I)), function(col) unique(I[, col]))

  lengths <- sapply(L, length)

  lengths == 2

}
