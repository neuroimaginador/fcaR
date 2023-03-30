first_time_message <- function(option, txt) {

  mess <- getOption(option)
  if (is.null(mess) || mess) {

    # txt <- paste0(
    #   "Note: You must include the following command in you LaTeX document:\n",
    #   "\\newcommand{\\el}[2]{\\ensuremath{^{#2\\!\\!}/{#1}}}"
    # )

    message(txt)

    L <- list(FALSE)
    names(L) <- option
    options(L)

  }

}


print_message <- function(msg) {

  glue::glue(msg, .envir = parent.frame()) |> cat()
  cat("\n")

}
