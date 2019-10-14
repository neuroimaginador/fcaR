create_fca_env <- function() {

  attached <- search()

  if ("fca_shims" %in% attached) return()

  base::attach(new.env(),
               name = "fca_shims",
               warn.conflicts = FALSE)

}

save_in_fca_env <- function(...) {

  create_fca_env()

  names <- as.character(substitute(list(...)))[-1L]

  envir <- parent.frame()

  for (i in seq_along(names)) {

    assign(x = names[i],
           value = envir[[names[i]]],
           envir = as.environment("fca_shims"))

  }

}

read_from_fca_env <- function(...) {

  create_fca_env()

  names <- as.character(substitute(list(...)))[-1L]

  envir <- as.environment("fca_shims")

  for (i in seq_along(names)) {

    assign(x = names[i],
           value = envir[[names[i]]],
           envir = parent.frame())

  }

}

remove_from_fca_env <- function(...) {

  create_fca_env()

  names <- as.character(substitute(list(...)))[-1L]

  rm(list = names, envir = as.environment("fca_shims"))

}
