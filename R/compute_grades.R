compute_grades <- function(I) {

  lapply(seq(ncol(I)),
         function(col) {
           v <- I[, col]
           g <- sort(unique(v))
           g <- g[g > 0]

           if (g[length(g)] < 1) g <- c(g, 1)

           g

         }
  )

}
