to_transactions.SpM <- function(I) {

  Matrix::sparseMatrix(i = I$pi, p = I$pp,
                       x = I$px, dims = c(I$pnrow, length(I$pp) - 1),
                       repr = "C") %>%
    methods::as("ngCMatrix") %>%
    methods::as("transactions")

}
