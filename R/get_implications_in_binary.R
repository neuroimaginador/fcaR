.get_implications_in_binary <- function(I) {

  grades_set <- sort(unique(as.vector(I)))
  grades_set <- grades_set[grades_set > 0]
  attributes <- colnames(I)

  if (grades_set[2] == 1) {

    # Crisp
    #
    on.exit({

      DGbasis <- read_from_fca_env(DGbasis)

      implications <- implication_set$new(name = "DGbasis",
                                          attributes = colnames(I),
                                          lhs = DGbasis$get_LHS_matrix(),
                                          rhs = DGbasis$get_RHS_matrix())

      save_in_fca_env(implications)

    })

    implications <- .get_dgbasis_binary_opt(I)

  } else {

    # Fuzzy

    expI <- .expand_dataset(I)

    imp_basis <- expI$implications
    my_I <- expI$I

    on.exit({

      DGbasis <- read_from_fca_env(DGbasis)

      LHS <- .recode_to_original_grades(t(DGbasis$get_LHS_matrix()), grades_set = grades_set[grades_set > 0])
      LHS <- Matrix(t(LHS), sparse = TRUE)

      RHS <- .recode_to_original_grades(t(DGbasis$get_RHS_matrix()), grades_set = grades_set[grades_set > 0])
      RHS <- Matrix(t(RHS), sparse = TRUE)

      implications <- implication_set$new(name = "DGbasis",
                                          attributes = colnames(I),
                                          lhs = LHS, rhs = RHS)

      save_in_fca_env(implications)

    })

    r <- .get_dgbasis_binary_opt(my_I, imp_basis = imp_basis)

    LHS <- .recode_to_original_grades(t(r$get_LHS_matrix()), grades_set = grades_set[grades_set > 0])
    LHS <- Matrix(t(LHS), sparse = TRUE)

    RHS <- .recode_to_original_grades(t(r$get_RHS_matrix()), grades_set = grades_set[grades_set > 0])
    RHS <- Matrix(t(RHS), sparse = TRUE)

    implications <- implication_set$new(name = "",
                                        attributes = attributes,
                                        lhs = LHS,
                                        rhs = RHS)

  }

  return(implications)

}
