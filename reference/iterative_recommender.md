# Iterative closure recommender (Simplification Logic)

Multi-round attribute recommender: at each step computes
`closure(S, reduce = TRUE)` (infer + prune the rule-base), then asks
about the still-unknown attribute that appears most often in the LHS of
the *active* implications.

## Usage

``` r
iterative_recommender(
  fc,
  initial = character(0),
  max_rounds = 4,
  interactive_mode = FALSE,
  verbose = TRUE
)
```

## Arguments

- fc:

  A `FormalContext` with implications already computed.

- initial:

  Character vector of attribute names known at the start.

- max_rounds:

  Maximum number of question rounds.

- interactive_mode:

  If `TRUE` and the session is interactive, ask Yes/No via
  [`menu()`](https://rdrr.io/r/utils/menu.html); otherwise auto-accept
  the top suggestion.

- verbose:

  Print progress each round.

## Value

Invisibly, a list with `query` (final `Set`) and `implications` (the
last pruned implication set).
