compute_grades <- function(I) {

  lapply(seq(ncol(I)),
         function(col) {
           v <- c(0, I[, col], 1)
           g <- sort(unique(v))
           # g <- g[g > 0]

           g

         }
  )

}
