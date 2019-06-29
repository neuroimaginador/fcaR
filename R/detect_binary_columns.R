.detect_binary_columns <- function(I) {

  sapply(seq(ncol(I)), function(col) all(sort(unique(I[, col])) == c(0, 1)))


}
