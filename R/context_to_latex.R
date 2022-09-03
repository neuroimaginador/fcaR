context_to_latex <- function(I,
                             objects = rownames(I),
                             attributes = colnames(I)) {

  objects <- format_label(objects)
  attributes <- format_label(attributes)

  if (fcaR_options("use_tabulary")) {

    format <- c("L",
                rep("C", length(attributes))) %>%
      stringr::str_flatten()

  } else {

    format <- c("l",
                rep("c", length(attributes))) %>%
      stringr::str_flatten()

  }


  objects <- objects %>%
    stringr::str_replace_all(pattern = stringr::fixed("["),
                             replacement = "{}[")

  header <- c("", attributes) %>%
    stringr::str_flatten(" & ")
  header <- paste0(header, "\\\\")

  rows <- c()
  for (i in seq_along(objects)) {

    this_row <- c(objects[i], I[i, ]) %>%
      stringr::str_flatten(" & ")

    rows <- c(rows, this_row)

  }

  rows <- rows %>%
    stringr::str_flatten("\\\\ \n")
  rows <- paste0(rows, "\\\\")

  body <- c("\\toprule",
            header,
            "\\midrule",
            rows,
            "\\bottomrule") %>%
    stringr::str_flatten("\n")

  if (fcaR_options("use_tabulary")) {

    tabular <- c(
      paste0("\\begin{tabulary}{0.9\\textwidth}{",
             format, "}"),
      body,
      "\\end{tabulary}") %>%
      stringr::str_flatten("\n")

  } else {

    tabular <- c(
      paste0("\\begin{tabular}{",
             format, "}"),
      body,
      "\\end{tabular}") %>%
      stringr::str_flatten("\n")

  }

  return(tabular)

}
