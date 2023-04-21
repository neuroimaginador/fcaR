set_to_latex <- function(S, attributes) {

  idx <- Matrix::which(S > 0)

  if (length(idx) > 0) {

    A <- S[idx]
    decimal_places <- fcaR_options("decimal_places")
    A <- A %>%
      formatC(digits = decimal_places) %>%
      stringr::str_replace_all("\\s*", "")

    att <- attributes[idx] %>%
      format_label()

    str <- sapply(seq(length(att)),
                  \(i) element_to_latex(nm = att[i],
                                        val = A[i])) %>%
      stringr::str_flatten(", ")

    str <- paste0("\\left\\{", str, "\\right\\}")
    if (fcaR_options("use_ensuremath")) {

      str <- paste0("\\ensuremath{", str, "}")

    }
    return(str)

  } else {

    return("\\varnothing")

  }

}

#' @importFrom glue glue
element_to_latex <- function(nm, val) {


  first_time_message(
    option = "fcaR.el_message",
    txt = paste0(
      "Note: You must include the following commands in you LaTeX document:\n",
      "\\usepackage{amsmath}",
      "\\newcommand{\\el}[2]{\\ensuremath{^{#2\\!\\!}/{#1}}}"
    ))

  if (!fcaR_options("escape_")) {

    fcaR_options("use_mathrm" = FALSE)
  }

  if (fcaR_options("use_mathrm")) {

    nm <- glue::glue("\\mathrm{{{nm}}}")

  }

  if (val == "1") {

    return(nm)

  } else {

    str <- glue::glue(
      "\\el{[nm]}{[val]}",
      # "{^{[val]}}\\!/[nm]",
      .open = "[", .close = "]"
    )

    return(str)

  }

}

imp_to_latex <- function(imp_set, ncols = 1,
                         numbered = TRUE,
                         numbers = seq(imp_set$cardinality())) {

  LHS <- imp_set$get_LHS_matrix()
  RHS <- imp_set$get_RHS_matrix()
  attributes <- imp_set$get_attributes()

  output <- c()

  for (i in seq(ncol(LHS))) {

    lhs <- Matrix::Matrix(LHS[, i], sparse = TRUE)
    rhs <- Matrix::Matrix(RHS[, i], sparse = TRUE)

    prefix <- ifelse(numbered, paste0(numbers[i], ": &"), "")
    output <- c(output,
                paste0(prefix,
                       set_to_latex(lhs, attributes), "&\\ensuremath{\\Rightarrow}&",
                       set_to_latex(rhs, attributes)))

  }

  remaining <- ncol(LHS) %% ncols

  if (remaining > 0) {

    remaining <- ncols - remaining
    output <- c(output, rep("", remaining))

  }

  output <- matrix(output, ncol = ncols)

  output <- sapply(seq(nrow(output)), function(r) {

    paste0(stringr::str_flatten(output[r, ], collapse = " & "), "\\\\")

  })

  format_cols <- ifelse(numbered, "rrcl", "rcl")

  sz <- fcaR_options("latex_size")
  if (sz != "normalsize") {

    size_prefix <- paste0("\\begingroup\\", sz)
    size_suffix <- "\\endgroup\n"

  } else {

    size_prefix <- size_suffix <- ""

  }

  output <- c(paste0(size_prefix,
                     "\\begin{longtable*}{",
                     stringr::str_flatten(rep(format_cols, ncols)),
                     "}"),
              output,
              paste0("\\end{longtable*}",
                     size_suffix))

  output <- paste(output, collapse = "\n")

  # cat(output)

  return(invisible(output))

}

concepts_to_latex <- function(extents, intents,
                              objects, attributes,
                              ncols = 1,
                              align = TRUE,
                              numbered = TRUE,
                              numbers = seq(ncol(extents))) {

  output <- c()
  n <- ncol(extents)

  for (i in seq(n)) {

    A <- Matrix::Matrix(extents[, i], sparse = TRUE)
    B <- Matrix::Matrix(intents[, i], sparse = TRUE)

    A <- Set$new(attributes = objects, M = A)
    B <- Set$new(attributes = attributes, M = B)

    prefix <- ifelse(numbered, paste0(numbers[i], ": &"), "")
    output <- c(output,
                paste0(prefix,
                       "$\\left(\\,",
                       A$to_latex(print = FALSE),
                       ",\\right.$",
                       ifelse(align, "&", "\\,"),
                       "$\\left.",
                       B$to_latex(print = FALSE),
                       "\\,\\right)$"))

  }

  output <- matrix(output, ncol = ncols)

  output <- sapply(seq(nrow(output)), function(r) {

    paste0(stringr::str_flatten(output[r, ], collapse = " & "), "\\\\")

  })

  format_cols <- c(ifelse(numbered, "l", ""),
                   ifelse(align, "ll", "l"))

  sz <- fcaR_options("latex_size")
  if (sz != "normalsize") {

    size_prefix <- paste0("\\begingroup\\", sz)
    size_suffix <- "\\endgroup\n"

  } else {

    size_prefix <- size_suffix <- ""

  }

  output <- c(paste0(size_prefix,
                     "\\begin{longtable*}{",
                     stringr::str_flatten(rep(format_cols, ncols)),
                     "}"),
              output,
              "\\end{longtable*}",
              size_suffix)

  output <- paste(output, collapse = "\n")

  # cat(output)

  return(invisible(output))

}
