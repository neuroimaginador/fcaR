reduce <- function(implic_set) {

  return(invisible(implic_set$apply_reduction()))

}

compose <- function(implic_set) {


  return(invisible(implic_set$apply_composition()))

}

simplify <- function(implic_set) {


  return(invisible(implic_set$apply_simplification()))

}

r_simplify <- function(implic_set) {


  return(invisible(implic_set$apply_r_simplification()))

}

