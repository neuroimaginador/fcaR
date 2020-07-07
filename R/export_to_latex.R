#' @importFrom stringr str_flatten
set_to_latex <- function(S, attributes) {

  idx <- which(S > 0)

  if (length(idx) > 0) {

    A <- S[idx]
    att <- attributes[idx]

    tmp <- paste0("\\ensuremath{\\left\\{",
                  str_flatten(paste0("{^{", A, "}}\\!/\\mathrm{", att, "}"),
                              collapse = ",\\, "), "\\right\\}}")

  } else {

    tmp <- "\\ensuremath{\\varnothing}"

  }

  tmp <- gsub(pattern = "(\\{\\^\\{1\\}\\}\\\\!\\/)",
              replacement = "",
              x = tmp)

  return(tmp)

}

#' @importFrom stringr str_flatten
#' @importFrom Matrix Matrix
imp_to_latex <- function(imp_set, ncols = 1,
                         numbered = TRUE,
                         numbers = seq(imp_set$cardinality())) {

  LHS <- imp_set$get_LHS_matrix()
  RHS <- imp_set$get_RHS_matrix()
  attributes <- imp_set$get_attributes()

  output <- c()

  for (i in seq(ncol(LHS))) {

    lhs <- Matrix(LHS[, i], sparse = TRUE)
    rhs <- Matrix(RHS[, i], sparse = TRUE)

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

    paste0(str_flatten(output[r, ], collapse = " & "), "\\\\")

  })

  format_cols <- ifelse(numbered, "rrcl", "rcl")

  output <- c(paste0("\\begin{longtable}{", str_flatten(rep(format_cols, ncols)), "}"), output, "\\end{longtable}")

  output <- paste(output, collapse = "\n")

  # cat(output)

  return(invisible(output))

}

#' @importFrom stringr str_flatten
concepts_to_latex <- function(concept_list,
                              ncols = 1,
                              align = TRUE,
                              numbered = TRUE,
                              numbers = seq_along(concept_list)) {

  output <- c()

  for (i in seq_along(concept_list)) {

    prefix <- ifelse(numbered, paste0(numbers[i], ": &"), "")
    output <- c(output,
                paste0(prefix,
                       "$\\left(\\,",
                       concept_list[[i]]$get_extent()$to_latex(print = FALSE),
                       ",\\right.$",
                       ifelse(align, "&", "\\,"),
                       "$\\left.",
                       concept_list[[i]]$get_intent()$to_latex(print = FALSE),
                       "\\,\\right)$"))

  }

  output <- matrix(output, ncol = ncols)

  output <- sapply(seq(nrow(output)), function(r) {

    paste0(str_flatten(output[r, ], collapse = " & "), "\\\\")

  })

  format_cols <- c(ifelse(numbered, "l", ""),
                   ifelse(align, "ll", "l"))

  output <- c(paste0("\\begin{longtable}{",
                     str_flatten(rep(format_cols, ncols)), "}"), output, "\\end{longtable}")

  output <- paste(output, collapse = "\n")

  # cat(output)

  return(invisible(output))

}
