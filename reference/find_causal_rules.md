# Causal Association Rules

Functions to mine causal association rules from a formal context.

## Usage

``` r
find_causal_rules(
  fc,
  response_var,
  min_support = 0.1,
  confidence_level = 0.95,
  max_length = 3,
  verbose = FALSE
)
```

## Arguments

- fc:

  A FormalContext object.

- response_var:

  (character) The name of the response variable.

- min_support:

  (numeric) Minimum support.

- confidence_level:

  (numeric) Confidence level for the causality test.

- max_length:

  (integer) Maximum length of the premise.

- verbose:

  (logical) Show verbose output?

## Value

A data frame with causal rules and their quality metrics.
