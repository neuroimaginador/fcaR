##%######################################################%##
#                                                          #
####               %holds% and %respects%               ####
#                                                          #
##%######################################################%##

#' Implications that hold in a Formal Context
#'
#' @param imps  (\code{ImplicationSet}) The set of implications to test if hold in the formal context.
#' @param fc    (\code{FormalContext}) A formal context where to test if the implications hold.
#'
#' @return A logical vector, indicating if each implication holds in the formal context.
#'
#' @examples
#' fc <- FormalContext$new(planets)
#' fc$find_implications()
#' imps <- fc$implications$clone()
#' imps %holds_in% fc
#'
#' @export
`%holds_in%` <- function(imps, fc) {

  premises <- imps$get_LHS_matrix()
  conclusions <- imps$get_RHS_matrix()
  I <- fc$incidence()

  holds <- sapply(seq(ncol(premises)),
                     function(i) {

                       p <- .extract_column(premises, i)
                       p <- compute_closure(p, I)
                       .subset(.extract_column(conclusions, i),
                               p)

                     }) %>%
    purrr::reduce(cbind) %>%
    as.vector()

  return(holds)

}


#' Check if Set or FormalContext respects an ImplicationSet
#'
#' @param set    (list of \code{Set}s, or a \code{FormalContext}) The sets of attributes to check whether they respect the \code{ImplicationSet}.
#' @param imps  (\code{ImplicationSet}) The set of implications to check.
#'
#' @return A logical matrix with as many rows as \code{Set}s and as many columns as implications in the \code{ImplicationSet}. A \code{TRUE} in element (i, j) of the result means that the i-th \code{Set} respects the j-th implication of the \code{ImplicationSet}.
#'
#' @export
#'
#' @examples
#' fc <- FormalContext$new(planets)
#' fc$find_implications()
#' imps <- fc$implications$clone()
#' fc %respects% imps
`%respects%` <- function(set, imps) {

  if (inherits(set, "Set")) {

    set <- list(set)

  }

  if (inherits(set, "FormalContext")) {

    S <- lapply(seq_along(set$objects),
                  function(i) {

                    Set$new(attributes = set$attributes,
                                  M = set$I[, i])

                  })
    set <- S

  }

  if (inherits(set, "list") &&
      (length(set) > 0) &&
      inherits(set[[1]], "Set")) {

    res <- lapply(set, function(s) .respect(s, imps)) %>%
      purrr::reduce(cbind)

    setnumber <- stringr::str_pad(seq_along(set),
                                  width = stringr::str_length(length(set)),
                                  side = "left",
                                  pad = "0")
    impnumber <- stringr::str_pad(seq(imps$cardinality()),
                                  width = stringr::str_length(imps$cardinality()),
                                  side = "left",
                                  pad = "0")

    colnames(res) <- paste0("set_", setnumber)
    rownames(res) <- paste0("imp_", impnumber)

    return(Matrix::t(res))

  }

  stop("Error: Not of the expected class.")

}

.respect <- function(S, imps) {

  .subset(imps$get_RHS_matrix(), S$get_vector()) |
    !(.subset(imps$get_LHS_matrix(), S$get_vector()))

}
