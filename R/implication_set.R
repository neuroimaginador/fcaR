#' import sets Matrix
implication_set <- R6::R6Class(

  classname = "ImplicationSet",

  public = list(

    # Initialize with an optional name
    initialize = function(name = "", attributes = c()) {

      private$name <- name
      private$attributes <- attributes

    },

    from_arules = function(arules_imp,
                           force = TRUE) {

      attributes <- arules_imp@lhs@itemInfo$labels
      private$attributes <- attributes

      name <- as.character(arules_imp@info$data)
      private$name <- name

      LHS <- arules_imp@lhs@data
      RHS <- arules_imp@rhs@data

      private$lhs_matrix <- as(LHS, "dgCMatrix")
      private$rhs_matrix <- as(RHS, "dgCMatrix")

      # Force conversion into implication object type
      if (force) {

        self$matrices_to_implications()

      }

    },

    compute_sparse_matrix = function() {

      R <- convert_implication_list_to_sparse(implication_list = private$implications,
                                              attrs = private$attributes)

      private$lhs_matrix <- R$LHS
      private$rhs_matrix <- R$RHS

      private$lhs_subsets <- NULL
      private$lhsrhs_subsets <- NULL

    },

    # Number of implications in the set
    length = function() {

      max(c(length(private$implications),
            ncol(private$lhs_matrix)))

    },

    # Add new implication
    add_implication = function(imp) {

      private$implications <- c(private$implications, imp)

    },

    get_implications = function() {

      private$implications

    },

    # Compute the sintactic closure of a set wrt the implications
    compute_closure = function(S) {

      .implications_closure(S, private$implications)

    },

    print = function() {

      n <- 1
      for (imp in private$implications) {

        cat("Rule ", n, ": ")
        imp$print()
        n <- n + 1

      }

    },

    get_LHS_matrix = function() {

      private$lhs_matrix

    },

    get_RHS_matrix = function() {

      private$rhs_matrix

    },

    matrices_to_implications = function() {

      private$implications <- lapply(seq(ncol(private$lhs_matrix)),
                                     function(i) {

                                       .convert_sparse_to_fuzzy(LHS = private$lhs_matrix[, i],
                                                                RHS = private$rhs_matrix[, i],
                                                                attributes = private$attributes)

                                     }
      )

    },

    compute_subsets = function(both = FALSE) {

      if (is.null(private$lhs_matrix)) {

        message("Sparse matrices not previously computed. Computing now...")

        self$compute_sparse_matrix()

      }

      private$lhs_subsets <- .is_subset_sparse(private$lhs_matrix)

      if (both) {

        S <- rbind(private$lhs_matrix, private$rhs_matrix)

        private$lhsrhs_subsets <- .is_subset_sparse(S)

      }

    },

    clean = function() {

      RHS <- private$rhs_matrix

      empties <- which(colSums(RHS) == 0)

      if (length(empties) > 0) {

        private$lhs_matrix <- private$lhs_matrix[, -empties]
        private$rhs_matrix <- private$rhs_matrix[, -empties]

        if (length(private$implications) > 0) {

          private$implications <- private$implications[-empties]

        }

      }

      # Find duplicates
      self$compute_subsets(both = TRUE)

      equals <- private$lhsrhs_subsets & t(private$lhsrhs_subsets)

      with_duplicates <- which(rowSums(equals) > 1)

      unique_with_duplicates <- apply(equals[, with_duplicates],
                                      2,
                                      which.max)

      duplicates <- setdiff(with_duplicates, unique_with_duplicates)

      if (length(duplicates) > 0) {

        private$lhs_matrix <- private$lhs_matrix[, -duplicates]
        private$rhs_matrix <- private$rhs_matrix[, -duplicates]

        if (length(private$implications) > 0) {

          private$implications <- private$implications[-duplicates]

        }

      }

    },

    apply_rules = function(rules) {

      for (r in rules) {

        private$implications <- .apply_rule(
          implication_list = private$implications,
          rule = r)

      }

    },

    apply_reduction = function() {

      if (is.null(private$lhs_matrix)) {

        message("Sparse matrices not previously computed. Computing now...")

        self$compute_sparse_matrix()

      }

      LHS <- as.matrix(private$lhs_matrix)
      RHS <- as.matrix(private$rhs_matrix)

      RHS <- apply_F_elementwise(x = RHS,
                                 y = LHS,
                                 type = "set_diff")

      # imps <- lapply(seq(ncol(LHS)), function(i) {
      #   .convert_sparse_to_fuzzy(LHS[, i],
      #                            RHS[, i],
      #                            private$attributes)})
      #
      # private$implications <- imps
      private$rhs_matrix <- Matrix::Matrix(RHS,
                                           sparse = TRUE)

      self$clean()

    },

    apply_composition = function() {

      if (is.null(private$lhs_subsets)) {

        message("Subset matrices not previously computed. Computing now...")

        self$compute_subsets()

      }

      logic_name <- tolower(fuzzy_logic()$name)

      LHS <- private$lhs_matrix
      RHS <- private$rhs_matrix
      implication_list <- list()

      equal_LHS <- private$lhs_subsets & t(private$lhs_subsets)

      replicas <- which(rowSums(equal_LHS) > 1)
      singles <- which(rowSums(equal_LHS) == 1)

      marked_to_remove <- rep(FALSE, ncol(LHS))

      if (length(replicas) > 0) {

        for (rep_id in seq_along(replicas)) {

          if (marked_to_remove[replicas[rep_id]]) next

          ids_to_merge <- which(equal_LHS[replicas[rep_id], ])


          A <- LHS[, ids_to_merge[1]]

          allRHS <- as.matrix(RHS[, ids_to_merge])

          B <- apply_F_rowwise(x = allRHS,
                               type = paste0(logic_name, "_S"),
                               init_value = 0)

          marked_to_remove[ids_to_merge] <- TRUE

          A <- gset(support = private$attributes, memberships = A)
          B <- gset(support = private$attributes, memberships = B)

          imp <- implication$new(lhs = A, rhs = B)

          implication_list <- c(implication_list, imp)

        }

      }

      if (length(singles) > 0) {

        for (i in singles) {

          A <- LHS[, i]
          B <- RHS[, i]
          A <- gset(support = private$attributes, memberships = A)
          B <- gset(support = private$attributes, memberships = B)

          imp <- implication$new(lhs = A, rhs = B)

          implication_list <- c(implication_list, imp)

        }

      }

      private$implications <- implication_list

      self$compute_sparse_matrix()

    },

    apply_simplification = function() {

      if (is.null(private$lhs_subsets)) {

        message("Subset matrices not previously computed. Computing now...")

        self$compute_subsets()

      }

      logic_name <- tolower(fuzzy_logic()$name)

      LHS <- private$lhs_matrix
      RHS <- private$rhs_matrix

      new_LHS <- Matrix(0,
                        nrow = length(private$attributes),
                        ncol = 1,
                        sparse = TRUE)

      new_RHS <- Matrix(0,
                        nrow = length(private$attributes),
                        ncol = 1,
                        sparse = TRUE)

      # implication_list <- list()

      are_subset <- which(rowSums(private$lhs_subsets) > 1)

      marked_as_single <- rep(TRUE, ncol(LHS))

      if (length(are_subset) > 0) {

        for (subs in seq_along(are_subset)) {

          this_row <- are_subset[subs]

          my_idx <- which(private$lhs_subsets[this_row, ])
          marked_as_single[my_idx] <- FALSE

          my_idx <- setdiff(my_idx, this_row)
          A <- as.matrix(LHS[, this_row])
          B <- as.matrix(RHS[, this_row])

          C <- as.matrix(LHS[, my_idx])
          # D <- as.matrix(RHS[, my_idx])

          C_B <- apply_F_rowwise_xy(x = C,
                                    y = B,
                                    type = "set_diff")

          AC_B <- apply_F_rowwise_xy(x = C_B,
                                     y = A,
                                     type = paste0(logic_name, "_S"))

          new_LHS <- cbind(new_LHS, AC_B)
          new_RHS <- cbind(new_RHS, RHS[, my_idx])

          # for (k in seq_along(my_idx)) {
          #
          #   newLHS <- gset(support = private$attributes,
          #                  memberships = AC_B[, k])
          #   newRHS <- gset(support = private$attributes,
          #                  memberships = as.matrix(RHS[, my_idx[k]]))
          #
          #   imp <- implication$new(lhs = newLHS, rhs = newRHS)
          #
          #   implication_list <- c(implication_list, imp)
          #
          # }
          #
          # A <- gset(support = private$attributes, memberships = as.vector(A))
          # B <- gset(support = private$attributes, memberships = as.vector(B))
          #
          # imp <- implication$new(lhs = A, rhs = B)
          #
          # implication_list <- c(implication_list, imp)


        }

      }

      singles <- which(marked_as_single)

      if (length(singles) > 0) {

        new_LHS <- cbind(new_LHS, LHS[, singles])
        new_RHS <- cbind(new_RHS, RHS[, singles])

      }

      # if (length(singles) > 0) {
      #
      #   for (i in singles) {
      #
      #     A <- LHS[, i]
      #     B <- RHS[, i]
      #     A <- gset(support = private$attributes, memberships = A)
      #     B <- gset(support = private$attributes, memberships = B)
      #
      #     imp <- implication$new(lhs = A, rhs = B)
      #
      #     implication_list <- c(implication_list, imp)
      #
      #   }

      # private$implications <- implication_list

      # self$compute_sparse_matrix()

      private$lhs_matrix <- new_LHS[, -1]
      private$rhs_matrix <- new_RHS[, -1]

      self$clean()

    },

    apply_r_simplification = function() {

      if (is.null(private$lhs_subsets)) {

        message("Subset matrices not previously computed. Computing now...")

        self$compute_subsets()

      }

      logic_name <- tolower(fuzzy_logic()$name)

      LHS <- private$lhs_matrix
      RHS <- private$rhs_matrix
      implication_list <- list()

      LHSURHS <- apply_F_elementwise(as.matrix(LHS),
                                     as.matrix(RHS),
                                     type = paste0(logic_name, "_S"))
      LHSURHS <- Matrix(LHSURHS, sparse = TRUE)

      AinCD <- .is_subset_sparse(x = LHS, y = LHSURHS)

      are_subset <- which(rowSums(AinCD) > 1)

      marked_as_single <- rep(TRUE, ncol(LHS))

      if (length(are_subset) > 0) {

        for (subs in seq_along(are_subset)) {

          this_row <- are_subset[subs]

          my_idx <- which(AinCD[this_row, ])
          marked_as_single[my_idx] <- FALSE

          my_idx <- setdiff(my_idx, this_row)
          A <- as.matrix(LHS[, this_row])
          B <- as.matrix(RHS[, this_row])

          C <- as.matrix(LHS[, my_idx])
          D <- as.matrix(RHS[, my_idx])


          new_D <- apply_F_rowwise_xy(x = D,
                                      y = B,
                                      type = "set_diff")

          for (k in seq_along(my_idx)) {

            newLHS <- gset(support = private$attributes,
                           memberships = C[, k])
            newRHS <- gset(support = private$attributes,
                           memberships = new_D[, k])

            imp <- implication$new(lhs = newLHS, rhs = newRHS)

            implication_list <- c(implication_list, imp)

          }

          A <- gset(support = private$attributes, memberships = as.vector(A))
          B <- gset(support = private$attributes, memberships = as.vector(B))

          imp <- implication$new(lhs = A, rhs = B)

          implication_list <- c(implication_list, imp)


        }

      }

      singles <- which(marked_as_single)

      if (length(singles) > 0) {

        for (i in singles) {

          A <- LHS[, i]
          B <- RHS[, i]
          A <- gset(support = private$attributes, memberships = A)
          B <- gset(support = private$attributes, memberships = B)

          imp <- implication$new(lhs = A, rhs = B)

          implication_list <- c(implication_list, imp)

        }

      }

      private$implications <- implication_list

      self$compute_sparse_matrix()
      self$clean()
      self$compute_sparse_matrix()

    },

    remove_redundancies = function() {

      # A -> B and C -> B, with A subset of C (axiom C -> A) => remove C -> B

      if (is.null(private$lhs_subsets)) {

        message("Subset matrices not previously computed. Computing now...")

        self$compute_subsets()

        message("Computed.")

      }

      # logic_name <- tolower(fuzzy_logic()$name)

      LHS <- private$lhs_matrix
      RHS <- private$rhs_matrix

      RHS_subsets <- .is_subset_sparse(RHS)

      equal_RHS <- RHS_subsets & t(RHS_subsets)

      same_rhs <- which(rowSums(equal_RHS) > 1)

      marked_as_single <- rep(TRUE, ncol(LHS))

      if (length(same_rhs) > 0) {

        for (k in seq_along(same_rhs)) {

          # Index for A -> B
          this_row <- same_rhs[k]

          idx_equal <- which(equal_RHS[this_row, ])
          idx_equal <- setdiff(idx_equal, this_row)

          my_idx <- which(private$lhs_subsets[this_row, idx_equal])

          my_idx <- idx_equal[my_idx]

          my_idx <- setdiff(my_idx, this_row)

          marked_as_single[my_idx] <- FALSE

        }

      }

      # Add singles
      singles <- which(marked_as_single)

      private$lhs_matrix <- private$lhs_matrix[, singles]
      private$rhs_matrix <- private$rhs_matrix[, singles]

      private$lhs_subsets <- private$lhs_subsets[singles, singles]

      if (length(private$implications) > 0) {

        private$implications <- private$implications[singles]

      }

      self$clean()

    }


  ),

  private = list(

    name = "",

    implications = list(),

    attributes = NULL,

    lhs_matrix = NULL,
    rhs_matrix = NULL,

    lhs_subsets = NULL,
    lhsrhs_subsets = NULL

  )

)
