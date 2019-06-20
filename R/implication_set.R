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
    from_arules = function(arules_imp) {

      attributes <- arules_imp@lhs@itemInfo$labels
      private$attributes <- attributes

      name <- as.character(arules_imp@info$data)
      private$name <- name

      private$lhs_matrix <- as(arules_imp@lhs@data, "dgCMatrix")
      private$rhs_matrix <- as(arules_imp@rhs@data, "dgCMatrix")

      rownames(private$lhs_matrix) <- private$attributes
      rownames(private$rhs_matrix) <- private$attributes

    },

    to_arules = function() {

      LHS <- as(private$lhs_matrix, "ngCMatrix")
      LHS <- as(LHS, "itemMatrix")
      itemLabels(LHS) <- private$attributes

      RHS <- as(private$rhs_matrix, "ngCMatrix")
      RHS <- as(RHS, "itemMatrix")
      itemLabels(RHS) <- private$attributes

      rules <- new("rules", lhs = LHS, rhs = RHS)

      return(rules)

    },

    # Number of implications in the set
    length = function() {

      ncol(private$lhs_matrix)

    },

    # Add new implication
    add_implication = function(lhs, rhs) {

      if (is.null(private$lhs_matrix)) {

        private$lhs_matrix <- lhs

      } else {

        private$lhs_matrix <- add_col(private$lhs_matrix, lhs)

      }

      if (is.null(private$rhs_matrix)) {

        private$rhs_matrix <- rhs

      } else {

        private$rhs_matrix <- add_col(private$rhs_matrix, rhs)

      }

      rownames(private$lhs_matrix) <- private$attributes
      rownames(private$rhs_matrix) <- private$attributes

    },

    # Compute the sintactic closure of a set wrt the implications
    compute_closure = function(S, reduce = FALSE) {

      .compute_closure(S,
                       LHS = private$lhs_matrix,
                       RHS = private$rhs_matrix,
                       reduce = reduce)

    },

    recommend = function(S, attribute_filter) {

      .recommend_attribute(S = S,
                           LHS = private$lhs_matrix,
                           RHS = private$rhs_matrix,
                           attribute_filter = attribute_filter)

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

    },

    # Get the sparse matrix for LHS
    get_LHS_matrix = function() {

      private$lhs_matrix

    },

    # Get the sparse matrix for RHS
    get_RHS_matrix = function() {

      private$rhs_matrix

    },


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

    batch_apply = function(rules = c("composition", "generalization"),
                           batch_size = 25000) {

      L <- .batch_apply(LHS = private$lhs_matrix,
                        RHS = private$rhs_matrix,
                        rules = rules,
                        batch_size = batch_size)

      private$lhs_matrix <- L$lhs
      private$rhs_matrix <- L$rhs

    },

    apply_rules = function(rules) {

      for (r in rules) {

        private$implications <- .apply_rule(
          implication_list = private$implications,
          rule = r)

      }

    },


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

          L <- .compose_lhs_rhs_equal(Matrix(C, sparse = TRUE),
                                      Matrix(new_D, sparse = TRUE))
          C <- L$lhs
          new_D <- L$rhs

          L <- .remove_redundancies_lhs_rhs_general(C,
                                                    new_D)
          C <- L$lhs
          new_D <- L$rhs

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

    }

  ),

  private = list(

    name = "",

    attributes = NULL,

    lhs_matrix = NULL,
    rhs_matrix = NULL

  )

)
