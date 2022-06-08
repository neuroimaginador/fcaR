format_label <- function(x) {

  x %>%
    stringr::str_replace_all(
      "([#%&_])",
      "\\\\\\1"
    )

}
