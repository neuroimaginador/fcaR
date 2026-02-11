# context_to_latex_old <- function(I,
#                                  objects = rownames(I),
#                                  attributes = colnames(I)) {
#
#   objects <- format_label(objects)
#   attributes <- format_label(attributes)
#
#   if (fcaR_options("use_tabulary")) {
#
#     first_time_message(
#       option = "fcaR.tabulary_message",
#       txt = paste0(
#         "Note: You must include the following command in you LaTeX document:\n",
#         "\\usepackage{tabulary}"
#       ))
#
#     format <- c("L",
#                 rep("C", length(attributes))) |>
#       stringr::str_flatten()
#
#   } else {
#
#     format <- c("l",
#                 rep("c", length(attributes))) |>
#       stringr::str_flatten()
#
#   }
#
#
#   objects <- objects |>
#     stringr::str_replace_all(pattern = stringr::fixed("["),
#                              replacement = "{}[")
#
#   header <- c("", attributes) |>
#     stringr::str_flatten(" & ")
#   header <- paste0(header, "\\\\")
#
#   rows <- c()
#   for (i in seq_along(objects)) {
#
#     this_row <- c(objects[i], I[i, ]) |>
#       stringr::str_flatten(" & ")
#
#     rows <- c(rows, this_row)
#
#   }
#
#   rows <- rows |>
#     stringr::str_flatten("\\\\ \n")
#   rows <- paste0(rows, "\\\\")
#
#   body <- c("\\toprule",
#             header,
#             "\\midrule",
#             rows,
#             "\\bottomrule") |>
#     stringr::str_flatten("\n")
#
#   if (fcaR_options("use_tabulary")) {
#
#     tabular <- c(
#       paste0("\\begin{tabulary}{0.9\\textwidth}{",
#              format, "}"),
#       body,
#       "\\end{tabulary}") |>
#       stringr::str_flatten("\n")
#
#   } else {
#
#     tabular <- c(
#       paste0("\\begin{tabular}{",
#              format, "}"),
#       body,
#       "\\end{tabular}") |>
#       stringr::str_flatten("\n")
#
#   }
#
#   return(tabular)
#
# }

context_to_latex <- function(I,
                             objects = rownames(I),
                             attributes = colnames(I),
                             char = FALSE,
                             colnames = TRUE,
                             rownames = TRUE,
                             rotated  = FALSE) {

  objects <- format_label(objects)
  attributes <- format_label(attributes)

  objects <- paste0("$", objects, "$")
  attributes <- paste0("$", attributes, "$")

  if (is.character(I)) {

    char <- TRUE
    Ic <- I

    regex_blanks <- "^\\s+$"
    regex_startingdollar <- "^\\$"
    regex_endingdollar <- "\\$$"

    Ic[] <- I |>
      stringr::str_remove_all(regex_blanks) |>
      stringr::str_remove_all(regex_startingdollar) |>
      stringr::str_remove_all(regex_endingdollar)

    Ic[] <- paste0("$", Ic, "$") |>
      stringr::str_remove_all("\\$\\$")

  }

  if (!char) {

    vals <- (1 - I)^0.4 |> round(2)
    Ic <- glue::glue("{vals},{vals},{vals}")
    Ic <- paste0("${\\textcolor[rgb]{", Ic, "}\\bullet}$")
    dim(Ic) <- dim(I)

    Ic[I == 0] <- ""
    Ic[I == 1] <- "$\\bullet$"

  }

  format <- stringr::str_flatten(rep("c", ncol(I)))

  body <- apply(Ic, 1,
                FUN = \(v) stringr::str_flatten(v, " & "))

  if (rownames) {

    body <- paste0(objects, " & ", body)
    format <- paste0("r|", format)

  }


  if (colnames) {

    body <- body |>
      stringr::str_flatten(" \\\\\n")

    if (rotated) {

      header <- paste0("\\rotatebox{90}{", attributes, "}") |>
        stringr::str_flatten(" & ")

      if (rownames) {

        header <- paste0(" & ", header)

      }

      header <- paste0(header, "\\\\ \n \\hline\n")

    } else {

      header <- attributes |>
        stringr::str_flatten(" & ")

      if (rownames) {

        header <- paste0(" & ", header)

      }

      header <- paste0(header, "\\\\ \n \\hline \n")

    }

  } else {

    body <- body |>
      stringr::str_flatten(" \\\\\n\\hline\n")
    header <- ""

  }

  if (fcaR_options("use_tabulary")) {

    first_time_message(
      option = "fcaR.tabulary_message",
      txt = paste0(
        "Note: You must include the following command in you LaTeX document:\n",
        "\\usepackage{tabulary}"
      ))

    format <- toupper(format)

    latex <- c(paste0("\\begin{tabulary}{", format, "}\n"),
               header,
               body, "\n",
               "\\end{tabulary}\n")

  } else {

    latex <- c(paste0("\\begin{tabular}{", format, "}\n"),
               header,
               body, "\n",
               "\\end{tabular}\n")

  }


  return(latex)

}
