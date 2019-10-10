.detect_binary_columns <- function(I) {

  sapply(seq(ncol(I)),

         function(col) {

           v <- sort(unique(I[, col]))

           res <- length(v) == 2

           if (res) {

             res <- all(v == c(0, 1))

           }

           return(res)

         }
  )


}
