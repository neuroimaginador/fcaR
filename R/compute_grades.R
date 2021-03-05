compute_grades <- function(I) {

  lapply(seq(ncol(I)),
         function(col) {
           v <- c(0, .extract_column(I, col)@x, 1)
           g <- sort(unique(v))
           # g <- g[g > 0]

           g

         }
  )

}
