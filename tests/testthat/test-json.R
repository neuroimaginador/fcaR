test_that("ImplicationSet to_json and from_json work", {
    # Skip if jsonlite not available
    skip_if_not_installed("jsonlite")

    # Create a simple ImplicationSet
    data("planets")
    fc <- FormalContext$new(planets)
    fc$find_implications()
    imps <- fc$implications

    # Export to JSON
    json_str <- imps$to_json()

    expect_type(json_str, "character")
    expect_true(jsonlite::validate(json_str))

    # Import from JSON
    imps2 <- implications_from_json(json_str)

    expect_s3_class(imps2, "ImplicationSet")
    expect_equal(imps2$cardinality(), imps$cardinality())
    expect_equal(imps2$get_attributes(), imps$get_attributes())

    # Check content similarity
    # We can check if LHS/RHS matrices are identical
    # Note: Internal representation might differ in order if not careful, but logic should be same

    expect_equal(
        as.matrix(imps$get_LHS_matrix()),
        as.matrix(imps2$get_LHS_matrix())
    )
    expect_equal(
        as.matrix(imps$get_RHS_matrix()),
        as.matrix(imps2$get_RHS_matrix())
    )

    # Test with empty set
    imps_empty <- ImplicationSet$new(attributes = imps$get_attributes())
    json_empty <- imps_empty$to_json()
    imps_empty2 <- implications_from_json(json_empty)

    expect_equal(imps_empty2$cardinality(), 0)
    expect_equal(imps_empty2$get_attributes(), imps$get_attributes())
})

test_that("RuleSet to_json and from_json work", {
    skip_if_not_installed("jsonlite")
    skip_if_not_installed("arules")

    # Create a simple RuleSet from arules or manually
    # Let's create one manually
    data("planets")
    fc <- FormalContext$new(planets)
    fc$find_implications()

    # Hack: create ruleset from implications to test generic structure
    # Or use add method
    rs <- RuleSet$new(attributes = fc$attributes)
    # But we need to populate it.
    # Let's use internal lists to build one

    LHS <- Matrix::Matrix(c(1, 0, 0, 1), nrow = 2, sparse = TRUE)
    RHS <- Matrix::Matrix(c(0, 1, 1, 0), nrow = 2, sparse = TRUE)
    attrs <- c("A", "B")
    qual <- data.frame(confidence = c(0.8, 1.0), support = c(0.2, 0.5))

    rs <- RuleSet$new(attributes = attrs, lhs = LHS, rhs = RHS, quality = qual)

    # Export
    json_str <- rs$to_json()
    expect_true(jsonlite::validate(json_str))

    # Import
    rs2 <- rules_from_json(json_str)

    expect_s3_class(rs2, "RuleSet")
    expect_equal(rs2$cardinality(), rs$cardinality())
    expect_equal(rs2$get_attributes(), rs$get_attributes())

    expect_equal(
        as.matrix(rs$get_LHS_matrix()),
        as.matrix(rs2$get_LHS_matrix())
    )
    expect_equal(
        unname(as.matrix(rs$get_RHS_matrix())),
        unname(as.matrix(rs2$get_RHS_matrix()))
    )

    # Check quality
    q1 <- rs$get_quality()
    q2 <- rs2$get_quality()

    expect_equal(q1$confidence, q2$confidence)
    expect_equal(q1$support, q2$support)
})

test_that("ConceptLattice to_json and from_json work", {
    skip_if_not_installed("jsonlite")

    # Create a context and lattice
    data("planets")
    fc <- FormalContext$new(planets)
    fc$find_concepts()
    cl <- fc$concepts

    # Export to JSON
    json_str <- cl$to_json()
    expect_type(json_str, "character")
    expect_true(jsonlite::validate(json_str))

    # Import from JSON
    cl2 <- lattice_from_json(json_str)

    expect_true(inherits(cl2, "ConceptLattice"))
    expect_equal(cl2$size(), cl$size())
    expect_equal(cl2$objects, cl$objects)
    expect_equal(cl2$attributes, cl$attributes)

    # Check extents and intents matrices
    expect_equal(
        unname(as.matrix(cl$extents())),
        unname(as.matrix(cl2$extents()))
    )
    expect_equal(
        unname(as.matrix(cl$intents())),
        unname(as.matrix(cl2$intents()))
    )

    # Verify hierarchy/edges export (JSON check)
    data <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
    expect_true(!is.null(data$hierarchy))
    expect_true(length(data$hierarchy) > 0)
})

test_that("FormalContext to_json and from_json work", {
    skip_if_not_installed("jsonlite")

    # Create a context
    data("planets")
    fc <- FormalContext$new(planets)
    fc$find_concepts()
    fc$find_implications()

    # Export to JSON
    json_str <- fc$to_json()
    expect_type(json_str, "character")
    expect_true(jsonlite::validate(json_str))

    # Import from JSON
    fc2 <- context_from_json(json_str)

    expect_true(inherits(fc2, "FormalContext"))
    expect_equal(fc2$objects, fc$objects)
    expect_equal(fc2$attributes, fc$attributes)

    # Check I
    expect_equal(unname(as.matrix(fc2$I)), unname(as.matrix(fc$I)))

    # Check nested concepts
    expect_true(inherits(fc2$concepts, "ConceptLattice"))
    expect_equal(fc2$concepts$size(), fc$concepts$size())
    expect_equal(
        unname(as.matrix(fc2$concepts$extents())),
        unname(as.matrix(fc$concepts$extents()))
    )

    # Check nested implications
    expect_true(inherits(fc2$implications, "ImplicationSet"))
    expect_equal(fc2$implications$cardinality(), fc$implications$cardinality())
    expect_equal(
        unname(as.matrix(fc2$implications$get_LHS_matrix())),
        unname(as.matrix(fc$implications$get_LHS_matrix()))
    )
})
