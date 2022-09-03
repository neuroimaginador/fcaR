convert_to_sparse <- function(M) {

  methods::as(methods::as(methods::as(M,
           "dMatrix"),
        "generalMatrix"),
     "CsparseMatrix")

}
