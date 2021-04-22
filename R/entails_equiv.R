##%######################################################%##
#                                                          #
####                 %entails% and %~%                  ####
#                                                          #
##%######################################################%##


#' Entailment between implication sets
#'
#' @param imps     (\code{ImplicationSet}) A set of implications.
#' @param imps2    (\code{ImplicationSet}) A set of implications which
#' is tested to check if it follows semantically from \code{imps}.
#'
#' @return A logical vector, where element k is \code{TRUE} if the
#'  k-th implication in \code{imps2} follows from \code{imps}.
#'
#' @export
#'
#' @examples
#' fc <- FormalContext$new(planets)
#' fc$find_implications()
#' imps <- fc$implications[1:4]$clone()
#' imps2 <- fc$implications[3:6]$clone()
#' imps %entails% imps2
`%entails%` <- function(imps, imps2) {

  conclusions <- imps2$get_RHS_matrix()
  premises <- imps2$get_LHS_matrix()

  entails <- sapply(seq(ncol(premises)),
                  function(i) {

                    p <- .extract_column(premises, i)
                    cl <- .compute_closure(
                      S = p,
                      LHS = imps$get_LHS_matrix(),
                      RHS = imps$get_RHS_matrix(),
                      attributes = imps$get_attributes())$closure

                    .subset(.extract_column(conclusions, i),
                            cl)

                  }) %>%
    purrr::reduce(cbind) %>%
    Matrix::as.matrix()

  return(entails)

}

#' Equivalence of sets of implications
#'
#' @param imps  A \code{ImplicationSet}.
#' @param imps2 Another \code{ImplicationSet}.
#'
#' @return \code{TRUE} of and only if \code{imps} and \code{imps2}
#' are equivalent, that is, if every implication in \code{imps}
#' follows from \code{imps2} and viceversa.
#'
#' @export
#'
#' @examples
#' fc <- FormalContext$new(planets)
#' fc$find_implications()
#' imps <- fc$implications$clone()
#' imps2 <- imps$clone()
#' imps2$apply_rules(c("simp", "rsimp"))
#' imps %~% imps2
#' imps %~% imps2[1:9]
`%~%` <- function(imps, imps2) {

  all(imps %entails% imps2) && all(imps2 %entails% imps)

}
