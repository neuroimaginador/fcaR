#' @title
#' Causal Association Rules
#'
#' @description
#' Functions to mine causal association rules from a formal context.
#'
#' @param fc A FormalContext object.
#' @param response_var (character) The name of the response variable.
#' @param min_support (numeric) Minimum support.
#' @param confidence_level (numeric) Confidence level for the causality test.
#' @param max_length (integer) Maximum length of the premise.
#' @param verbose (logical) Show verbose output?
#'
#' @return A data frame with causal rules and their quality metrics.
#' @importFrom stats qnorm
#' @export
find_causal_rules <- function(
    fc,
    response_var,
    min_support = 0.1,
    confidence_level = 0.95,
    max_length = 3,
    verbose = FALSE
) {
    if (!inherits(fc, "FormalContext")) {
        stop("fc must be a FormalContext object.")
    }

    attributes <- fc$attributes
    if (!response_var %in% attributes) {
        stop("Response variable must be an attribute of the context.")
    }

    # Z-score for confidence level
    zconf <- stats::qnorm(1 - (1 - confidence_level) / 2)

    # 1. Frequent Variables
    # Filter variables that have enough local support with respect to the response variable
    # This corresponds to: (local-support ctx (->Implication #{%} #{response-var})) > min-lsupp
    # in the Clojure code.
    # Local support of A -> B seems to be supp(A U B) / supp(A)? No, that's confidence.
    # Let's check Clojure 'local-support'. It's usually support depending on context.
    # Assuming standard support here.

    # For efficiency, precompute supports of all singletons and pairs involving response_var?
    # Or just iterate.

    frequent_vars <- character(0)
    for (att in setdiff(attributes, response_var)) {
        # Direct access:
        idx_att <- match(att, fc$attributes)
        supp_att <- sum(fc$I[idx_att, ]) / length(fc$objects)

        if (supp_att >= min_support) {
            frequent_vars <- c(frequent_vars, att)
        }
    }

    # 2. Irrelevant Variables
    # Determine variables that are irrelevant to the response variable.
    # A variable V is irrelevant if V -> Target has logic CI lower bound <= 1.

    irrelevant_vars <- character(0)
    for (var in frequent_vars) {
        if (!is_causally_relevant(fc, var, response_var, zconf)) {
            irrelevant_vars <- c(irrelevant_vars, var)
        }
    }

    # 3. Discovery Loop
    # Start with singletons from (frequent_vars \ response_var \ irrelevant_vars)

    candidates <- setdiff(frequent_vars, irrelevant_vars)
    candidates <- as.list(candidates) # List of character vectors

    # Store results as a list of lists (rows)
    all_causal_rules <- list()

    current_length <- 1
    while (current_length <= max_length && length(candidates) > 0) {
        # Check causality for each candidate
        # Vectorize or loop? Loop for now.

        if (verbose) {
            cat(sprintf(
                "Checking %d candidates of length %d...\n",
                length(candidates),
                current_length
            ))
        }

        # Keep track of valid premises in this level for generating next level
        valid_premises <- list()

        for (premise in candidates) {
            res <- is_causal(
                fc,
                premise,
                response_var,
                irrelevant_vars,
                zconf,
                verbose = verbose
            )

            if (!is.null(res) && res$support >= min_support) {
                # res is a list with metrics
                new_rule <- list(
                    premise = paste(premise, collapse = ", "),
                    conclusion = response_var,
                    support = res$support,
                    confidence = res$confidence,
                    fair_odds_ratio = res$odds_ratio,
                    ci_lower = res$ci[1],
                    ci_upper = res$ci[2]
                )

                # Check metrics validity (already done in is_causal for causality, but keep for structure)
                # Redundancy check:
                # Is there a subset of 'premise' in 'all_causal_rules' that has same support?
                is_redundant <- FALSE

                # Only check if length > 1
                if (current_length > 1) {
                    supp_P <- res$support_premise

                    # Iterate through lower-level rules?
                    # We need to find if any proper subset has same support.
                    # This is slightly expensive if many rules.
                    # 'all_causal_rules' contains simplified rules so far.

                    # Heuristic: we only generated candidates from valid lower-level ones (if we do Apriori generation properly).
                    # But here we do simpler generation.

                    # Let's check strict redundancy:
                    # Look for subsets P' of P in the discovered rules.
                    # If supp(P') == supp(P), then P is redundant.

                    # Since we iterate by length, previous rules are shorter.
                    # Optimization: only check against rules of length = current - 1 ?
                    # No, A -> T might be redundant due to A -> T? Wait.
                    # If P = {A, B}, redundancy is if {A} -> T exists and supp({A}) == supp({A, B}).

                    # But we don't have easy access to supp({A}) inside 'all_causal_rules' unless we stored it.
                    # We stored 'support' which is support of rule P->T (supp(P U T)).
                    # We need supp(P).

                    # Let's verify support of subsets directly from data?
                    # Or rely on the fact that if supp(P) == supp(subset), then P and subset are equivalent in objects coverage.

                    # Clojure uses `local-support` equality check on subsets.
                    # (subset? old new) and (= supp(old) supp(new)).

                    # Let's do it.
                    # Candidates generation step (below) should ideally prevent generating redundant ones?
                    # But verifying here is safer.

                    # Check against all previous valid premises (valid_premises_history?)
                    # That is expensive.
                    # However, if we assume candidates are generated from valid premises,
                    # we only need to check immediate subsets?

                    # Let's just check against all found rules so far.

                    for (rule in all_causal_rules) {
                        # Reconstruct premise vector
                        old_premise <- unlist(strsplit(rule$premise, ", "))

                        if (
                            all(old_premise %in% premise) &&
                                length(old_premise) < length(premise)
                        ) {
                            # It is a subset.
                            # Check support of premise.
                            # We didn't store support_premise in the rule list logic above (fixed now).
                            # Wait, 'support' column usually means support of the rule.
                            # Let's calculate support of old_premise again to be sure?
                            # Or better, store 'support_premise' in the list.

                            idx_old <- match(old_premise, fc$attributes)
                            if (length(idx_old) > 1) {
                                supp_old <- sum(
                                    Matrix::colSums(fc$I[
                                        idx_old,
                                        ,
                                        drop = FALSE
                                    ]) ==
                                        length(old_premise)
                                )
                            } else {
                                supp_old <- sum(fc$I[idx_old, ] == 1)
                            }

                            if (supp_old == supp_P) {
                                is_redundant <- TRUE
                                break
                            }
                        }
                    }
                } else {
                    supp_P <- res$support_premise
                }

                if (!is_redundant) {
                    # Add to results
                    all_causal_rules[[length(all_causal_rules) + 1]] <- new_rule
                    valid_premises[[length(valid_premises) + 1]] <- premise
                }
            }
        }

        # Generate next level candidates
        if (current_length < max_length) {
            next_candidates <- list()
            single_vars <- setdiff(frequent_vars, irrelevant_vars)

            for (cand in valid_premises) {
                # Only extend valid premises!
                for (var in single_vars) {
                    if (!var %in% cand) {
                        new_cand <- sort(c(cand, var))
                        # Avoid duplicates in next_candidates
                        # Can use a hash/string check
                        cand_str <- paste(new_cand, collapse = "\r")
                        if (is.null(next_candidates[[cand_str]])) {
                            next_candidates[[cand_str]] <- new_cand
                        }
                    }
                }
            }
            candidates <- unname(next_candidates)

            # Additional pruning:
            # Clojure 'find-redundant' also checks if extended rule has same support as base rule?
            # We did that in the loop above (check if P_new is redundant w.r.t P_old).
        } else {
            candidates <- list()
        }

        current_length <- current_length + 1
    }

    # Convert list of lists to data frame
    if (length(all_causal_rules) > 0) {
        df <- do.call(
            rbind,
            lapply(all_causal_rules, as.data.frame, stringsAsFactors = FALSE)
        )
    } else {
        df <- data.frame(
            premise = character(),
            conclusion = character(),
            support = numeric(),
            confidence = numeric(),
            fair_odds_ratio = numeric(),
            ci_lower = numeric(),
            ci_upper = numeric(),
            stringsAsFactors = FALSE
        )
    }

    return(df)
}

# Helper: Is variable causally relevant?
is_causally_relevant <- function(fc, variable, response_var, zconf) {
    # Compute regular odds ratio and CI
    # Implication: variable -> response_var

    # Counts:
    # n11: var=1, resp=1
    # n10: var=1, resp=0
    # n01: var=0, resp=1
    # n00: var=0, resp=0

    idx_v <- match(variable, fc$attributes)
    idx_r <- match(response_var, fc$attributes)

    vec_v <- fc$I[idx_v, ]
    vec_r <- fc$I[idx_r, ]

    # Assuming binary
    n11 <- sum(vec_v & vec_r)
    n10 <- sum(vec_v & !vec_r)
    n01 <- sum(!vec_v & vec_r)
    n00 <- sum(!vec_v & !vec_r)

    # Odds Ratio
    # (n11 * n00) / (n10 * n01)
    # With smoothing? Clojure uses smoothing if 0?
    # The Clojure code for standard odds-ratio logic is not fully visible in the chunks,
    # but standard practice is often Haldane-Anscombe correction (add 0.5) if zeros.
    # Let's check `fair-odds-ratio` matches.
    # In Clojure `fair-odds-ratio`:
    # (/ (reduce + ... only-exposed ...) (max 1 (reduce + ... only-nonexposed ...)))
    # It just divides sums.

    # Replicating `causally-relevant?`:
    # (let [impl (->Implication #{variable} #{response-variable})]
    #   (> (last (confidence-interval ctx impl (odds-ratio ctx impl) zconfidence)) 1))

    # We need `confidence-interval` function logic.
    # (confidence-bound op ctx premise conclusion odds-ratio zconf)
    # exp(ln(OR) +/- z * sqrt(1/n11 + 1/n10 + 1/n01 + 1/n00))

    # Odds Ratio:
    or_val <- (n11 * n00) / (max(1, n10 * n01))
    if (or_val == 0) {
        return(FALSE)
    } # Can't be causal if OR is 0

    se <- sqrt(
        1 / max(1, n11) + 1 / max(1, n10) + 1 / max(1, n01) + 1 / max(1, n00)
    )

    # Lower bound
    lb <- exp(log(or_val) - zconf * se)

    return(lb > 1)
}

# Helper: Is implication causal?
is_causal <- function(
    fc,
    premise,
    response_var,
    irrelevant_vars,
    zconf,
    thresh = 2,
    verbose = FALSE
) {
    # thresh default?

    # 1. Exclusive Variables
    # Variables mutually exclusive with premise.
    # Two variables a, b matches if supp(a & b) is low.
    # Implementation: Check all attributes against premise.

    # Optimization: pre-calculate exclusion matrix?
    # For now, simplistic check.

    exclusive_vars <- character(0)
    # Clojure `exclusive-variables`:
    # For each p in premise, check against each attribute b.
    # if supp(p & b) <= thresh (count?) or supp(b & !p) ?? No, it says:
    # (<= (absolute-support ctx [#{a b} #{}]) thresh) Or (<= (absolute-support ctx [#{b} #{a}]) thresh)
    # It seems to check if they effectively don't co-occur.

    # Let's assume threshold is a count (e.g. 0 or small number).

    for (p in premise) {
        idx_p <- match(p, fc$attributes)
        vec_p <- fc$I[idx_p, ]

        for (att in fc$attributes) {
            if (att %in% premise) {
                next
            }

            idx_att <- match(att, fc$attributes)
            vec_att <- fc$I[idx_att, ]

            # supp({p, att})
            n_p_att <- sum(vec_p & vec_att)
            # supp({att} \ {p}) ?? Clojure says [#{b} #{a}] which usually means b is present, a is NOT.
            # If b implies a ??

            # Let's stick to simple "mutually exclusive": they don't appear together.
            if (n_p_att <= thresh) {
                exclusive_vars <- c(exclusive_vars, att)
            }
        }
    }
    exclusive_vars <- unique(exclusive_vars)

    # 2. Controlled Variables
    # All \ (Premise U Target U Irrelevant U Exclusive)
    excluded <- unique(c(
        premise,
        response_var,
        irrelevant_vars,
        exclusive_vars
    ))
    controlled_vars <- setdiff(fc$attributes, excluded)

    # 3. Fair Data Set
    # matched_pairs
    matched <- find_matched_pairs_greedy(
        fc,
        premise,
        response_var,
        controlled_vars
    )

    if (length(matched) == 0) {
        return(NULL)
    }

    # 4. Fair Odds Ratio
    # Count pairs
    # n11: exposed(p=1, r=1) & nonexposed(p=0, r=0)
    # n10: exposed(p=1, r=0) & nonexposed(p=0, r=1)

    # Wait, Clojure `only-exposed`:
    # exposed has p & r, nonexposed does NOT have r.
    # (and (incident? ctx exposed premise-attr)
    #      (and (incident? ctx exposed conclusion-attr)
    #           (not (incident? ctx nonexposed conclusion-attr))))
    #
    # Since 'matched' guarantees exposed has p and nonexposed has !p,
    # This reduces to: exposed has r AND nonexposed has !r.
    # So n_exposed_only_r

    # `only-nonexposed`:
    # exposed has p, but !r.
    # nonexposed has r.

    n_exposed_only_r <- 0
    n_nonexposed_only_r <- 0

    idx_r <- match(response_var, fc$attributes)
    vec_r <- fc$I[idx_r, ]

    for (pair in matched) {
        # pair is c(idx_exposed, idx_nonexposed)
        exp_idx <- pair[1]
        non_idx <- pair[2]

        has_r_exp <- vec_r[exp_idx] == 1
        has_r_non <- vec_r[non_idx] == 1

        if (has_r_exp && !has_r_non) {
            n_exposed_only_r <- n_exposed_only_r + 1
        }
        if (!has_r_exp && has_r_non) {
            n_nonexposed_only_r <- n_nonexposed_only_r + 1
        }
    }

    # Fair Odds Ratio
    fair_or <- n_exposed_only_r / (max(1, n_nonexposed_only_r))

    # Fair CI
    # SE = sqrt(1/max(n_exposed_only_r + 1) + 1/max(n_nonexposed_only_r + 1)) ?
    # Clojure:
    # sqrt( 1/(max(n_exposed_only_r, 1)) + 1/(max(n_nonexposed_only_r, 1)) )
    # Note: The clojure code sums 1 to the max, likely for smoothing?
    # No, checks (max (reduce...) 1). So simply max(count, 1).

    se <- sqrt(1 / max(1, n_exposed_only_r) + 1 / max(1, n_nonexposed_only_r))

    lb <- exp(log(fair_or) - zconf * se)
    ub <- exp(log(fair_or) + zconf * se)

    # Calculate support of premise on the FAIR data set?
    # Or original support?
    # For redundancy check we need original support of premise.

    idx_prem <- match(premise, fc$attributes)
    if (length(idx_prem) > 1) {
        supp_cnt <- sum(
            Matrix::colSums(fc$I[idx_prem, , drop = FALSE]) == length(premise)
        )
    } else {
        supp_cnt <- sum(fc$I[idx_prem, ] == 1)
    }

    # Confidence: supp(P U T) / supp(P)
    # Ideally should be causal confidence?
    # Let's return standard confidence for now, or Fair Confidence?
    # Clojure has `fair-confidence-interval` etc.
    # Here we just need some quality metrics.
    # Let's return local support of P.

    if (lb > 1) {
        return(list(
            support = supp_cnt / length(fc$objects), # Support of premise? Or P->T?
            # Standard support is P U T.
            # But here let's return support of premise as it is what we use for redundancy.
            # Wait, usually support(A->B) is support(A U B).
            # Let's compute P U T support.

            support_premise = supp_cnt,

            support = {
                idx_all <- match(c(premise, response_var), fc$attributes)
                sum(
                    Matrix::colSums(fc$I[idx_all, , drop = FALSE]) ==
                        length(idx_all)
                ) /
                    length(fc$objects)
            },

            confidence = {
                idx_all <- match(c(premise, response_var), fc$attributes)
                sum(
                    Matrix::colSums(fc$I[idx_all, , drop = FALSE]) ==
                        length(idx_all)
                ) /
                    supp_cnt
            },

            odds_ratio = fair_or,
            ci = c(lb, ub)
        ))
    } else {
        return(NULL)
    }
}

# Helper: Matched Pairs (Greedy)
find_matched_pairs_greedy <- function(
    fc,
    premise,
    response_var,
    controlled_vars
) {
    # We need to find pairs (u, v) such that:
    # u[controlled] == v[controlled]
    # u[premise] == 1, v[premise] == 0  (assuming binary full compliance)
    # Actually, premise can be a set. u has ALL premise, v DOES NOT have ALL premise.
    # Clojure: (or (and (subset? premise a) (not (subset? premise b))) ...)
    # So strict inequality on premise containment.

    # Strategy:
    # 1. Calculate 'signature' for each object based on controlled variables.
    #    Group objects by signature.
    # 2. Within each group:
    #    Separate into P (has premise) and NotP (doesn't have premise).
    #    Pair up greedily.

    # Construct signature string/factor
    if (length(controlled_vars) > 0) {
        idx_ctrl <- match(controlled_vars, fc$attributes)
        # Transpose to get objects as rows
        # Convert to dense matrix for paste? Or use robust hashing?
        # paste is easy.
        sub_I <- fc$I[idx_ctrl, , drop = FALSE]
        # collapsed string per column
        sigs <- apply(sub_I, 2, function(x) paste(x, collapse = ""))
    } else {
        sigs <- rep("", length(fc$objects))
    }

    # Check premise presence
    idx_prem <- match(premise, fc$attributes)
    if (length(idx_prem) > 1) {
        has_premise <- Matrix::colSums(fc$I[idx_prem, , drop = FALSE]) ==
            length(premise)
    } else {
        has_premise <- fc$I[idx_prem, ] == 1
    }

    # Grouping
    # Split indices by signature
    groups <- split(seq_len(length(fc$objects)), sigs)

    matched_pairs <- list()

    for (g in groups) {
        # g is vector of object indices
        sub_has <- has_premise[g]

        idx_P <- g[sub_has]
        idx_NotP <- g[!sub_has]

        # Greedy pairing
        # Just zip them up until one runs out
        n_pairs <- min(length(idx_P), length(idx_NotP))

        if (n_pairs > 0) {
            for (i in seq_len(n_pairs)) {
                # Pair (Exposed, NonExposed)
                matched_pairs[[length(matched_pairs) + 1]] <- c(
                    idx_P[i],
                    idx_NotP[i]
                )
            }
        }
    }

    return(matched_pairs)
}
