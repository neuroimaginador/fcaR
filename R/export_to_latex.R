print_set_latex <- function(S, attributes) {

  idx <- which(S > 0)

  if (length(idx) > 0) {

    A <- S[idx]
    att <- attributes[idx]

    tmp <- paste0("\\{",
                  str_flatten(paste0("{{", A, "}}/", att),
                              collapse = ",\\, "), "\\}")

  } else {

    "\\{\\}"

  }

}

imp_to_latex <- function(imp_set, ncols = 1) {

  LHS <- imp_set$get_LHS_matrix()
  RHS <- imp_set$get_RHS_matrix()
  attributes <- imp_set$.__enclos_env__$private$attributes

  output <- c()

  for (i in seq(ncol(LHS))) {

    lhs <- Matrix(LHS[, i], sparse = TRUE)
    rhs <- Matrix(RHS[, i], sparse = TRUE)

    output <- c(output,
    paste0(print_set_latex(lhs, attributes), "&\\Rightarrow&",
           print_set_latex(rhs, attributes)))

  }

  output <- matrix(output, ncol = ncols)

  output <- sapply(seq(nrow(output)), function(r) {

    paste0(str_flatten(output[r, ], collapse = " & "), "\\\\")

  })

  output <- c(paste0("\\begin{array}{", str_flatten(rep("rcl", ncols)), "}"), output, "\\end{array}")

  cat(output, sep = "\n")
}

