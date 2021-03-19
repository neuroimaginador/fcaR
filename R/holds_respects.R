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

  holds <- sapply(seq(ncol.SpM(premises)),
                     function(i) {

                       p <- extract_columns(premises, i)
                       p <- compute_closureSpM(p, I)
                       return(length(subsetSpM(extract_columns(conclusions, i),
                               p)$pi) > 0)

                     })
    # as.vector()

  return(holds)

}


#' Check if SparseSet or FormalContext respects an ImplicationSet
#'
#' @param set    (list of \code{SparseSet}s, or a \code{FormalContext}) The sets of attributes to check whether they respect the \code{ImplicationSet}.
#' @param imps  (\code{ImplicationSet}) The set of implications to check.
#'
#' @return A logical matrix with as many rows as \code{SparseSet}s and as many columns as implications in the \code{ImplicationSet}. A \code{TRUE} in element (i, j) of the result means that the i-th \code{SparseSet} respects the j-th implication of the \code{ImplicationSet}.
#'
#' @export
#' @importFrom stringr str_pad str_length
#'
#' @examples
#' fc <- FormalContext$new(planets)
#' fc$find_implications()
#' imps <- fc$implications$clone()
#' fc %respects% imps
`%respects%` <- function(set, imps) {

  if (inherits(set, "SparseSet")) {

    set <- list(set)

  }

  if (inherits(set, "FormalContext")) {

    S <- lapply(seq_along(set$objects),
                  function(i) {

                    SparseSet$new(attributes = set$attributes,
                                  M = set$I %>% extract_columns(i))

                  })
    set <- S

  }

  if (inherits(set, "list") &&
      (length(set) > 0) &&
      inherits(set[[1]], "SparseSet")) {

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

    return(t(res))

  }

  stop("Error: Not of the expected class.")

}

.respect <- function(S, imps) {

  ((to_matrix.SpM(subsetSpM(imps$get_RHS_matrix(), S$get_vector()))) |
    (1 - to_matrix.SpM(subsetSpM(imps$get_LHS_matrix(), S$get_vector())))) %>%
    as.logical()

  # subsetSpM(imps$get_RHS_matrix(), S$get_vector()) |
  #   !(.subset(imps$get_LHS_matrix(), S$get_vector()))

}
