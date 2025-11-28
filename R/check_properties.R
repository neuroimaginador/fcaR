#' Check Distributivity
#' @noRd
check_distributivity_internal <- function(meet, join) {
  check_distributivity_sparse(meet@i, meet@p, meet@x,
                              join@i, join@p, join@x, meet@Dim)
}

#' Check Modularity
#' @noRd
check_modularity_internal <- function(meet, join) {
  check_modularity_sparse(meet@i, meet@p, meet@x,
                          join@i, join@p, join@x, meet@Dim)
}

#' Check Semimodularity
#' @noRd
check_semimodularity_internal <- function(meet, join, covering) {
  cov <- methods::as(covering, "dgCMatrix")
  check_semimodularity_sparse(meet@i, meet@p, meet@x,
                              join@i, join@p, join@x,
                              cov@i, cov@p, cov@Dim)
}

#' Check Atomicity
#' @noRd
check_atomicity_internal <- function(adjacency, covering) {
  adj <- methods::as(adjacency, "nMatrix")
  cov <- methods::as(covering, "nMatrix")

  check_atomicity_sparse(adj@i, adj@p,
                         cov@i, cov@p, adj@Dim)
}
