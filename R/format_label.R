format_label <- function(x) {

  if (fcaR_options("escape_")) {

    regex <- "([#%&_])"

  } else {

    regex <- "([#%&])"

  }

  x %>%
    stringr::str_replace_all(
      regex,
      "\\\\\\1"
    )

}
