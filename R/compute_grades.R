compute_grades <- function(I) {

  lapply(seq(ncol.SpM(I)),
         function(col) {
           v <- c(0, extract_columns(I, col)$px, 1)
           g <- sort(unique(v))
           g

         }
  )

}
