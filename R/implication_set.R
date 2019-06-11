#' import sets Matrix
implication_set <- R6::R6Class(

  classname = "ImplicationSet",

  public = list(

    # Initialize with an optional name
    initialize = function(name = "", attributes = c()) {

      private$name <- name
      private$attributes <- attributes

    },

    # Import from arules object
    from_arules = function(arules_imp,
                           force = TRUE) {

      attributes <- arules_imp@lhs@itemInfo$labels
      private$attributes <- attributes

      name <- as.character(arules_imp@info$data)
      private$name <- name

      private$lhs_matrix <- as(arules_imp@lhs@data, "dgCMatrix")
      private$rhs_matrix <- as(arules_imp@rhs@data, "dgCMatrix")

      # # Force conversion into implication object type
      # if (force) {
      #
      #   self$matrices_to_implications()
      #
      # }

    },

    # # Compute LHS and RHS sparse matrices
    # compute_sparse_matrix = function() {
    #
    #   R <- convert_implication_list_to_sparse(implication_list = private$implications,
    #                                           attrs = private$attributes)
    #
    #   private$lhs_matrix <- R$LHS
    #   private$rhs_matrix <- R$RHS
    #
    #   private$lhs_subsets <- NULL
    #   private$lhsrhs_subsets <- NULL
    #
    # },

    # Number of implications in the set
    length = function() {

      ncol(private$lhs_matrix)

    },

    # Add new implication
    add_implication = function(lhs, rhs) {

      if (is.null(private$lhs_matrix)) {

        private$lhs_matrix <- lhs

      } else {

        private$lhs_matrix <- cbind(private$lhs_matrix, lhs)

      }

      if (is.null(private$rhs_matrix)) {

        private$rhs_matrix <- rhs

      } else {

        private$rhs_matrix <- cbind(private$rhs_matrix, rhs)

      }

      # private$implications <- c(private$implications, imp)

    },

    # # Get implications
    # get_implications = function() {
    #
    #   private$implications
    #
    # },

    # Compute the sintactic closure of a set wrt the implications
    compute_closure = function(S) {

      .compute_closure(S,
                       LHS = private$lhs_matrix,
                       RHS = private$rhs_matrix)

    },

    # Print all implications to output
    print = function() {

      n_implications <- ncol(private$lhs_matrix)
      attributes <- private$attributes
      LHS <- private$lhs_matrix
      RHS <- private$rhs_matrix

      implications <- sapply(seq(n_implications),
        function(i) paste0("Rule ", i, ": ",
                           .implication_to_string(LHS[, i], RHS[, i], attributes)))

      cat(implications, sep = "\n")

      # if (!(length(private$implications) == ncol(private$lhs_matrix))) {
      #
      #   private$matrices_to_implications()
      #
      # }
      #
      # n <- 1
      # for (imp in private$implications) {
      #
      #   cat("Rule ", n, ": ")
      #   imp$print()
      #   n <- n + 1
      #
      # }

    },

    # Get the sparse matrix for LHS
    get_LHS_matrix = function() {

      private$lhs_matrix

    },

    # Get the sparse matrix for RHS
    get_RHS_matrix = function() {

      private$rhs_matrix

    },

    # compute_subsets = function(both = FALSE) {
    #
    #   if (is.null(private$lhs_matrix)) {
    #
    #     message("Sparse matrices not previously computed. Computing now...")
    #
    #     self$compute_sparse_matrix()
    #
    #   }
    #
    #   private$lhs_subsets <- .is_subset_sparse(private$lhs_matrix)
    #
    #   if (both) {
    #
    #     S <- rbind(private$lhs_matrix, private$rhs_matrix)
    #
    #     private$lhsrhs_subsets <- .is_subset_sparse(S)
    #
    #   }
    #
    # },

    compute_intersections = function() {

      if (is.null(private$lhs_matrix)) {

        message("Sparse matrices not previously computed. Computing now...")

        self$compute_sparse_matrix()

      }

      logic_name <- tolower(fuzzy_logic()$name)

      private$lhs_intersect_rhs <-
        apply_F_elementwise(x = as.matrix(private$lhs_matrix),
                            y = as.matrix(private$rhs_matrix),
                            type = paste0(logic_name, "_T"))

    },

    # clean = function(duplicated = FALSE) {
    #
    #   empties <- which(colSums(private$rhs_matrix) == 0)
    #
    #   if (length(empties) > 0) {
    #
    #     private$lhs_matrix <- private$lhs_matrix[, -empties]
    #     private$rhs_matrix <- private$rhs_matrix[, -empties]
    #
    #   }
    #
    #   if (duplicated) {
    #
    #     # Find duplicates
    #     self$compute_subsets(both = TRUE)
    #
    #     equals <- private$lhsrhs_subsets & t(private$lhsrhs_subsets)
    #
    #     with_duplicates <- which(rowSums(equals) > 1)
    #
    #     unique_with_duplicates <- apply(equals[, with_duplicates],
    #                                     2,
    #                                     which.max)
    #
    #     duplicates <- setdiff(with_duplicates, unique_with_duplicates)
    #
    #     if (length(duplicates) > 0) {
    #
    #       private$lhs_matrix <- private$lhs_matrix[, -duplicates]
    #       private$rhs_matrix <- private$rhs_matrix[, -duplicates]
    #
    #       if (length(private$implications) > 0) {
    #
    #         private$implications <- private$implications[-duplicates]
    #
    #       }
    #
    #     }
    #
    #   }
    #
    # },

    batch_apply = function(rules = c("composition", "generalization"),
                           batch_size = 25000) {

      L <- .batch_apply(LHS = private$lhs_matrix,
                        RHS = private$rhs_matrix,
                        rules = rules,
                        batch_size = batch_size)

      private$lhs_matrix <- L$lhs
      private$rhs_matrix <- L$rhs

    },

    # apply_rules = function(rules) {
    #
    #   for (r in rules) {
    #
    #     private$implications <- .apply_rule(
    #       implication_list = private$implications,
    #       rule = r)
    #
    #   }
    #
    # },

    # apply_reduction = function() {
    #
    #   if (is.null(private$lhs_matrix)) {
    #
    #     message("Sparse matrices not previously computed. Computing now...")
    #
    #     self$compute_sparse_matrix()
    #
    #   }
    #
    #   L <- .reduce_lhs_rhs(private$lhs_matrix,
    #                        private$rhs_matrix)
    #
    #   private$rhs_matrix <- L$RHS
    #
    #   self$clean()
    #
    # },
    #
    # apply_composition = function() {
    #
    #   L <- .compose_lhs_rhs(private$lhs_matrix,
    #                         private$rhs_matrix)
    #
    #   private$lhs_matrix <- L$lhs
    #   private$rhs_matrix <- L$rhs
    #
    # },

    # apply_simplification_IJAR = function() {
    #
    #   if (is.null(private$lhs_subsets)) {
    #
    #     message("Subset matrices not previously computed. Computing now...")
    #
    #     self$compute_subsets()
    #
    #   }
    #
    #   logic_name <- tolower(fuzzy_logic()$name)
    #
    #   LHS <- private$lhs_matrix
    #   RHS <- private$rhs_matrix
    #
    #   new_LHS <- Matrix(0,
    #                     nrow = length(private$attributes),
    #                     ncol = 1,
    #                     sparse = TRUE)
    #
    #   new_RHS <- Matrix(0,
    #                     nrow = length(private$attributes),
    #                     ncol = 1,
    #                     sparse = TRUE)
    #
    #   # implication_list <- list()
    #
    #   are_subset <- which(rowSums(private$lhs_subsets) > 1)
    #
    #   marked_as_single <- rep(TRUE, ncol(LHS))
    #
    #   if (length(are_subset) > 0) {
    #
    #     for (subs in seq_along(are_subset)) {
    #
    #       this_row <- are_subset[subs]
    #
    #       my_idx <- which(private$lhs_subsets[this_row, ])
    #       marked_as_single[my_idx] <- FALSE
    #
    #       my_idx <- setdiff(my_idx, this_row)
    #       A <- as.matrix(LHS[, this_row])
    #       B <- as.matrix(RHS[, this_row])
    #
    #       C <- as.matrix(LHS[, my_idx])
    #       # D <- as.matrix(RHS[, my_idx])
    #
    #       C_B <- apply_F_rowwise_xy(x = C,
    #                                 y = B,
    #                                 type = "set_diff")
    #
    #       AC_B <- apply_F_rowwise_xy(x = C_B,
    #                                  y = A,
    #                                  type = paste0(logic_name, "_S"))
    #
    #       new_LHS <- cbind(new_LHS, AC_B)
    #       new_RHS <- cbind(new_RHS, RHS[, my_idx])
    #
    #     }
    #
    #   }
    #
    #   singles <- which(marked_as_single)
    #
    #   if (length(singles) > 0) {
    #
    #     new_LHS <- cbind(new_LHS, LHS[, singles])
    #     new_RHS <- cbind(new_RHS, RHS[, singles])
    #
    #   }
    #
    #   private$lhs_matrix <- new_LHS[, -1]
    #   private$rhs_matrix <- new_RHS[, -1]
    #
    #   private$lhs_subsets <- NULL
    #   private$lhs_intersect_rhs <- NULL
    #
    #   self$clean()
    #
    # },
    #
    # apply_simplification = function() {
    #
    #   L <- .simplify_lhs_rhs(private$lhs_matrix,
    #                          private$rhs_matrix)
    #
    #   private$lhs_matrix <- L$lhs
    #   private$rhs_matrix <- L$rhs
    #
    #   #
    #   #       if (is.null(private$lhs_subsets)) {
    #   #
    #   #         message("Subset matrices not previously computed. Computing now...")
    #   #
    #   #         self$compute_subsets()
    #   #
    #   #         message("Computed.")
    #   #
    #   #       }
    #   #
    #   #       if (is.null(private$lhs_intersect_rhs)) {
    #   #
    #   #         message("Intersection matrices not previously computed. Computing now...")
    #   #
    #   #         self$compute_intersections()
    #   #
    #   #         message("Computed.")
    #   #
    #   #       }
    #   #
    #   #       LHS <- private$lhs_matrix
    #   #       RHS <- private$rhs_matrix
    #   #
    #   #       new_LHS <- Matrix(0,
    #   #                         nrow = length(private$attributes),
    #   #                         ncol = 1,
    #   #                         sparse = TRUE)
    #   #
    #   #       new_RHS <- Matrix(0,
    #   #                         nrow = length(private$attributes),
    #   #                         ncol = 1,
    #   #                         sparse = TRUE)
    #   #
    #   #       # This gives the LHS that are subsets of other LHS
    #   #       condition1 <- rowSums(private$lhs_subsets) > 1
    #   #
    #   #       # This gives those LHS which are disjoint to their RHS
    #   #       condition2 <- colSums(private$lhs_intersect_rhs) == 0
    #   #
    #   #       are_subset <- which(condition1 & condition2)
    #   #
    #   #       marked_as_single <- rep(TRUE, ncol(LHS))
    #   #
    #   #       if (length(are_subset) > 0) {
    #   #
    #   #         for (subs in seq_along(are_subset)) {
    #   #
    #   #           this_row <- are_subset[subs]
    #   #
    #   #           my_idx <- which(private$lhs_subsets[this_row, ])
    #   #           my_idx <- setdiff(my_idx, this_row)
    #   #           marked_as_single[my_idx] <- FALSE
    #   #
    #   #           B <- as.matrix(RHS[, this_row])
    #   #
    #   #           C <- as.matrix(LHS[, my_idx])
    #   #           D <- as.matrix(RHS[, my_idx])
    #   #
    #   #           C_B <- apply_F_rowwise_xy(x = C,
    #   #                                     y = B,
    #   #                                     type = "set_diff")
    #   #
    #   #           D_B <- apply_F_rowwise_xy(x = D,
    #   #                                     y = B,
    #   #                                     type = "set_diff")
    #   #
    #   #           new_LHS <- cbind(new_LHS, C_B)
    #   #           new_RHS <- cbind(new_RHS, D_B)
    #   #
    #   #         }
    #   #
    #   #       }
    #   #
    #   #       singles <- which(marked_as_single)
    #   #
    #   #       if (length(singles) > 0) {
    #   #
    #   #         new_LHS <- cbind(new_LHS, LHS[, singles])
    #   #         new_RHS <- cbind(new_RHS, RHS[, singles])
    #   #
    #   #       }
    #   #
    #   #       private$lhs_matrix <- new_LHS[, -1]
    #   #       private$rhs_matrix <- new_RHS[, -1]
    #   #
    #   #       private$lhs_subsets <- NULL
    #   #       private$lhs_intersect_rhs <- NULL
    #   #
    #   #       self$clean()
    #
    # },

    apply_r_simplification = function() {

      if (is.null(private$lhs_subsets)) {

        message("Subset matrices not previously computed. Computing now...")

        self$compute_subsets()

      }

      if (is.null(private$lhs_intersect_rhs)) {

        message("Intersection matrices not previously computed. Computing now...")

        self$compute_intersections()

        message("Computed.")

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

      LHSURHS <- apply_F_elementwise(as.matrix(LHS),
                                     as.matrix(RHS),
                                     type = paste0(logic_name, "_S"))
      LHSURHS <- Matrix(LHSURHS, sparse = TRUE)

      AinCD <- .is_subset_sparse(x = LHS, y = LHSURHS)

      # This gives when A (from A -> B) is subset of CD (from C -> D)
      condition1 <- rowSums(AinCD) > 1

      # This gives those LHS which are disjoint to their RHS
      condition2 <- colSums(private$lhs_intersect_rhs) == 0

      are_subset <- which(condition1 & condition2)

      marked_as_single <- rep(TRUE, ncol(LHS))

      if (length(are_subset) > 0) {

        for (subs in seq_along(are_subset)) {

          this_row <- are_subset[subs]

          my_idx <- which(AinCD[this_row, ])
          my_idx <- setdiff(my_idx, this_row)
          marked_as_single[my_idx] <- FALSE

          # A <- as.matrix(LHS[, this_row])
          B <- as.matrix(RHS[, this_row])

          C <- as.matrix(LHS[, my_idx])
          D <- as.matrix(RHS[, my_idx])

          new_D <- apply_F_rowwise_xy(x = D,
                                      y = B,
                                      type = "set_diff")

          L <- .compose_lhs_rhs(Matrix(C, sparse = TRUE),
                                Matrix(new_D, sparse = TRUE))
          C <- L$lhs
          new_D <- L$rhs

          L <- .remove_redundancies_lhs_rhs_general(C,
                                                    new_D)
          C <- L$lhs
          new_D <- L$rhs

          # cat("From ", length(my_idx), " to ", ncol(C), "\n")

          new_LHS <- cbind(new_LHS, C)
          new_RHS <- cbind(new_RHS, new_D)

        }

      }

      singles <- which(marked_as_single)

      if (length(singles) > 0) {

        new_LHS <- cbind(new_LHS, LHS[, singles])
        new_RHS <- cbind(new_RHS, RHS[, singles])

      }

      private$lhs_matrix <- new_LHS[, -1]
      private$rhs_matrix <- new_RHS[, -1]

      private$lhs_subsets <- NULL
      private$lhs_intersect_rhs <- NULL

      self$clean()

    }

    # remove_redundancies = function() {
    #
    #   # A -> B and C -> B, with A subset of C (axiom C -> A)
    #   # => remove C -> B
    #
    #   L <- .remove_redundancies_lhs_rhs_general(private$lhs_matrix,
    #                                             private$rhs_matrix)
    #
    #   private$lhs_matrix <- L$lhs
    #   private$rhs_matrix <- L$rhs
    #
    # }

  ),

  private = list(

    name = "",

    # implications = list(),

    # # Convert the sparse matrices to implication objects
    # matrices_to_implications = function() {
    #
    #   private$implications <- lapply(seq(ncol(private$lhs_matrix)),
    #                                  function(i) {
    #
    #                                    .convert_sparse_to_fuzzy(LHS = private$lhs_matrix[, i],
    #                                                             RHS = private$rhs_matrix[, i],
    #                                                             attributes = private$attributes)
    #
    #                                  }
    #   )
    #
    # },

    attributes = NULL,

    lhs_matrix = NULL,
    rhs_matrix = NULL,

    tmp_lhs = NULL,
    tmp_rhs = NULL,

    lhs_subsets = NULL,
    lhsrhs_subsets = NULL,

    lhs_intersect_rhs = NULL

  )

)
