#' @importFrom sets .I.
.intent <- function(A, I) {

  # Use the derivation expression
  ms <- sapply(seq(ncol(I)),

               # The minimum of (A(g) -> I(g, m))
               function(m) {

                 min(
                   .I.(
                     as.matrix(A),
                     I[ , m]
                   )
                 )

               }

  )

  # Return a fuzzy set of the attributes,
  # with the computed memberships
  Matrix(ms, ncol = 1, sparse = TRUE)

}

#' @importFrom sets .I.
.extent <- function(B, I) {

  # Compute the memberships of the derived extention
  ms <- sapply(seq(nrow(I)),

               # The minimum of (B(m) -> I(g, m))
               function(g) {

                 min(
                   .I.(
                     as.matrix(B),
                     I[g, ]
                   )
                 )

               }

  )

  # Return a fuzzy set of the objects with the
  # computed attributes
  Matrix(ms, ncol = 1, sparse = TRUE)

}

.closure <- function(B, I) {

  # Double derivative starting from a fuzzy set of attributes.
  .intent(.extent(B, I), I)

}
