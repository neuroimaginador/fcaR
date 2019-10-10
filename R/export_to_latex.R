#' @import stringr
#' @export
print_set_latex <- function(S, attributes) {

  idx <- which(S > 0)

  if (length(idx) > 0) {

    A <- S[idx]
    att <- attributes[idx]

    tmp <- paste0("\\ensuremath{\\{",
                  str_flatten(paste0("{^{", A, "}}\\!/", att),
                              collapse = ",\\, "), "\\}}")

  } else {

    tmp <- "\\{\\}"

  }

  tmp <- gsub(pattern = "(\\{\\^\\{1\\}\\}\\\\!\\/)",
              replacement = "",
              x = tmp)

  return(tmp)

}

#' @import stringr
#' @export
imp_to_latex <- function(imp_set, ncols = 1) {

  LHS <- imp_set$get_LHS_matrix()
  RHS <- imp_set$get_RHS_matrix()
  attributes <- imp_set$get_attributes()

  output <- c()

  for (i in seq(ncol(LHS))) {

    lhs <- Matrix(LHS[, i], sparse = TRUE)
    rhs <- Matrix(RHS[, i], sparse = TRUE)

    output <- c(output,
    paste0(print_set_latex(lhs, attributes), "&\\ensuremath{\\Rightarrow}&",
           print_set_latex(rhs, attributes)))

  }

  output <- matrix(output, ncol = ncols)

  output <- sapply(seq(nrow(output)), function(r) {

    paste0(str_flatten(output[r, ], collapse = " & "), "\\\\")

  })

  output <- c(paste0("\\begin{array}{", str_flatten(rep("rcl", ncols)), "}"), output, "\\end{array}")

  output <- paste0("$$\n",
                   paste(output, collapse = "\n"),
                   "\n$$")

  cat(output)

  return(invisible(output))

}

