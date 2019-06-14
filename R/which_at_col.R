# Used to get which elements of a given column of a sparse
# matrix are nonzero.
which_at_col <- function(M, col) {

  if (M@p[col + 1] == M@p[col]) return(integer(0))

  M@i[seq(M@p[col] + 1, M@p[col + 1])] + 1

}
