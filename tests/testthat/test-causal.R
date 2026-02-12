test_that("Causal Association Rules work", {
    # Create a context where Treatment -> Recovery is causal
    # We need a confounding variable "Severity"
    # Structure:
    # Severity -> Treatment (doctors treat severe cases)
    # Severity -> Recovery (severe cases recover less)
    # Treatment -> Recovery (treatment helps)

    # Let's construct a dataset
    n <- 1000
    severity <- sample(c(0, 1), n, replace = TRUE)

    # Treatment depends on severity
    treatment_prob <- ifelse(severity == 1, 0.8, 0.2)
    treatment <- rbinom(n, 1, treatment_prob)

    # Recovery depends on severity and treatment
    # P(R=1) = base + 0.5*T - 0.3*S
    recovery_prob <- 0.3 + 0.5 * treatment - 0.2 * severity
    recovery_prob <- pmin(pmax(recovery_prob, 0), 1)
    recovery <- rbinom(n, 1, recovery_prob)

    # Create formal context
    I <- matrix(c(treatment, severity, recovery), ncol = 3)
    colnames(I) <- c("Treatment", "Severity", "Recovery")

    fc <- FormalContext$new(I)

    # Should find Treatment -> Recovery
    rules <- fc$find_causal_rules(
        response_var = "Recovery",
        confidence_level = 0.95,
        min_support = 0.1
    )

    expect_true(inherits(rules, "RuleSet"))
    expect_true(rules$cardinality() > 0)
    expect_output(print(rules))

    quality <- rules$get_quality()
    expect_true(is.data.frame(quality))
    expect_true(all(
        c(
            "support",
            "confidence",
            "fair_odds_ratio",
            "ci_lower",
            "ci_upper"
        ) %in%
            names(quality)
    ))

    # Check if "Treatment" is in the results
    # We can check LHS matrix
    lhs <- rules$get_LHS_matrix()
    expect_true(any(lhs["Treatment", ] > 0))
})


test_that("Causal Association Rules handle Simpson's Paradox", {
    # Example: Kidney stone treatment
    # Treatment A (New, expensive) given to large stones (severe)
    # Treatment B (Old, cheap) given to small stones (mild)
    # A is better, but looks worse because it treats harder cases.

    # Simplified version:
    # Response: Drowning
    # Factor: IceCream (correlated but not causal)
    # Confounder: Heat (causes both IceCream    # Construct Spurious Data
    set.seed(123)
    n <- 1000
    # Heat: 50%
    heat <- c(rep(1, 500), rep(0, 500))

    # Ice Cream: Depends strongly on Heat (80% if Hot, 20% if Cold)
    ice_cream <- numeric(1000)
    ice_cream[1:500] <- rbinom(500, 1, 0.8)
    ice_cream[501:1000] <- rbinom(500, 1, 0.2)

    # Drowning: Depends strongly on Heat (80% if Hot, 20% if Cold), indep of Ice Cream
    drowning <- numeric(1000)
    drowning[1:500] <- rbinom(500, 1, 0.8)
    drowning[501:1000] <- rbinom(500, 1, 0.2)

    I_spurious <- matrix(c(heat, ice_cream, drowning), ncol = 3)
    colnames(I_spurious) <- c("Heat", "IceCream", "Drowning")

    fc_spurious <- FormalContext$new(I_spurious)

    # Should find: Heat -> Drowning
    # Should NOT find: IceCream -> Drowning

    rules <- fc_spurious$find_causal_rules(
        response_var = "Drowning",
        min_support = 0.05
    )

    # Debug
    # rules$print()

    lhs <- rules$get_LHS_matrix()

    has_heat <- any(lhs["Heat", ] > 0)

    # Check if IceCream ALONE is a cause
    # We look for a rule where premise is "IceCream" (and nothing else)
    # i.e. column in LHS where IceCream is 1 and sum of that column is 1.
    ic_idx <- match("IceCream", rownames(lhs))
    has_ic_only <- any(lhs[ic_idx, ] > 0 & Matrix::colSums(lhs) == 1)

    expect_true(has_heat)
    expect_false(has_ic_only)
})

test_that("Causal Association Rules redundancy check works", {
    # Scenario: A -> T is causal. B is a superset of A (always present when A is, and more).
    # Wait, if B is always present when A is, then A subset B (objects).
    # Extent(A) contained in Extent(B).

    # Let's simple redundancy:
    # A -> T.
    # {A, B} -> T.
    # If we just add random B to the context.
    # A -> T (valid).
    # {A, B} -> T (valid if B doesn't contradict).
    # If B is irrelevant noise, {A, B} support will be lower.
    # We want {A, B} support == A support.
    # Requires B to cover all objects that A covers.
    # i.e., A => B implication holds. A is subset of B (concept-wise, attribute-wise B is subset of A?).
    # No, attribute-wise A implies B.

    # Let's construct:
    # A: 1 1 1 0 0 0
    # B: 1 1 1 1 1 0 (B is more frequent, covers A)
    # T: 1 1 1 0 0 0 (Perfect correlation with A)

    calc_A <- c(rep(1, 50), rep(0, 50))
    calc_B <- c(rep(1, 80), rep(0, 20)) # Covers A's 1s
    calc_T <- calc_A

    I <- matrix(c(calc_A, calc_B, calc_T), ncol = 3)
    colnames(I) <- c("A", "B", "T")

    fc <- FormalContext$new(I)

    # A -> T has support 0.5.
    # {A, B} -> T.
    # Supp({A, B}) is intersection of A and B. Which is A (since A subset B).
    # So supp({A, B}) == supp(A) == 0.5.

    # So {A, B} -> T should be redundant w.r.t A -> T.

    # Also B -> T?
    # B has 1s where T is 0. So B -> T is weak confidence. (50/80).

    # So we expect:
    # A -> T (found)
    # {A, B} -> T (redundant, not found)

    # What about Causal logic?
    # A -> T. Controlled: B.
    # Group B=1: A varies?
    # In B=1 (80 objects): A is 1 (50 objs), A is 0 (30 objs).
    # T follows A.
    # Pairs found. OR high. A -> T valid.

    rules <- fc$find_causal_rules(response_var = "T", min_support = 0.1)

    # Check premises
    lhs <- rules$get_LHS_matrix()

    # A is in some rule
    expect_true(any(lhs["A", ] > 0))

    # {A, B} should NOT be a rule
    both_A_B <- (lhs["A", ] > 0) & (lhs["B", ] > 0)
    expect_false(any(both_A_B))
})
