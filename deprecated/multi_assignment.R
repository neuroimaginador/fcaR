`:=` <- function(lhs, rhs) {

  LHS <- as.character(substitute(lhs))[-1L]

  is_matrix <- is.matrix(rhs)
  is_list <- is.list(rhs)
  is_vector <- is.vector(rhs)

  if (length(LHS) == 1) {

    assign(x = LHS, value = rhs, envir = parent.frame())

  } else {

    if (length(LHS) <= length(rhs)) {

      for (i in seq_along(LHS)) {

        if (is_valid_variable_name(LHS[i])) {

          if (is_list)
            assign(x = LHS[i], value = rhs[[i]], envir = parent.frame())

          if (is_matrix)
            assign(x = LHS[i], value = rhs[, i], envir = parent.frame())

          if (is_vector & !is_list)
            assign(x = LHS[i], value = rhs[i], envir = parent.frame())

        }

      }

    }

  }

}

conc <- function(lhs, rhs) {

  LHS <- as.character(substitute(lhs))

  is_matrix <- is.matrix(rhs)
  is_df <- is.data.frame(rhs)
  is_list <- is.list(rhs)
  is_vector <- is.vector(rhs)

  tmp <- get(x = LHS, envir = parent.frame())

  if (is_df | is_matrix) {

    assign(x = LHS, value = cbind(tmp, rhs), envir = parent.frame())

  }

  if (is_list) {

    assign(x = LHS, value = c(tmp, rhs), envir = parent.frame())

  }

}

# https://www.r-bloggers.com/testing-for-valid-variable-names/
is_valid_variable_name <- function(x, allow_reserved = TRUE, unique = FALSE) {

  ok <- rep.int(TRUE, length(x))

  #is name too long?
  max_name_length <- if(getRversion() < "2.13.0") 256L else 10000L

  #is it a reserved variable, i.e.
  #an ellipsis or two dots then a number?
  if(!allow_reserved)
  {
    ok[x == "..."] <- FALSE
    ok[grepl("^\\.{2}[[:digit:]]+$", x)] <- FALSE
  }

  #are names valid (and maybe unique)
  ok[x != make.names(x, unique = unique)] <- FALSE

  ok

}
