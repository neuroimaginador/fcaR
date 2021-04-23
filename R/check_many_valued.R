check_many_valued <- function(I) {

  suppressWarnings({
    val <- I %>%
      as.data.frame() %>%
      unlist() %>%
      as.vector() %>%
      as.numeric()})
  # val <- I$px
  return(any(is.na(val) | val > 1 | val < 0))

}

error_many_valued <- function() {

  stop("This formal context is many-valued, and this operation needs it to be binary or fuzzy.",
       call. = FALSE)

}
