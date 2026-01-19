# Data Manipulation with fcaR and dplyr

## Introduction

Formal Concept Analysis (FCA) typically involves a workflow of cleaning
data, extracting concepts, and analyzing implications. The **fcaR**
package integrates seamlessly with **dplyr**, allowing you to use the
“grammar of data manipulation” directly on `FormalContext` and
`ImplicationSet` objects.

This integration provides S3 methods for:

- **FormalContext**: `select`, `filter`, `mutate`, `arrange`, `rename`.
- **ImplicationSet**: `filter`, `arrange`, `slice`.

## Setup

First, load the necessary packages and the example dataset `planets`.

``` r
library(fcaR)
library(dplyr)

data("planets")
# Create the initial Formal Context
fc <- FormalContext$new(planets)
```

## Part 1: Context Manipulation

Real-world data is rarely ready for FCA out of the box. You might need
to derive new attributes, remove noise, or rename variables.

### 1.1 Renaming and Feature Engineering

We can use
[`rename()`](https://dplyr.tidyverse.org/reference/rename.html) to
standardize attribute names and
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) to
create new attributes based on logic applied to existing ones. This is
particularly powerful for **conceptual scaling** or creating
higher-level abstractions.

``` r
# Let's clean up the context
fc_clean <- fc %>% 
  rename(
    has_moon = moon,
    no_moon  = no_moon,
    is_large = large,
    is_small = small
  ) %>%
  mutate(
    # Create a new binary attribute 'giant_loner'
    # (A planet that is large but has no moon)
    giant_loner = is_large == 1 & no_moon == 1,
    
    # Create 'extreme_size' (either small or large)
    extreme_size = is_small == 1 | is_large == 1
  )

# Check the new attributes
print(fc_clean$attributes)
#> [1] "is_small"     "medium"       "is_large"     "near"         "far"         
#> [6] "has_moon"     "no_moon"      "giant_loner"  "extreme_size"
```

### 1.2 Filtering and Selecting

We can filter the *objects* (rows) and select *attributes* (columns) to
focus our analysis on a specific subset of the domain.

``` r
# Focus only on 'extreme' sized planets and keep specific attributes
fc_focused <- fc_clean %>%
  filter(extreme_size == 1) %>% 
  select(has_moon, giant_loner, is_large)

fc_focused$print()
#> FormalContext with 7 objects and 3 attributes.
#>          has_moon  giant_loner  is_large  
#>  Mercury                                  
#>    Venus                                  
#>    Earth     X                            
#>     Mars     X                            
#>  Jupiter     X                      X     
#>   Saturn     X                      X     
#>    Pluto     X
```

## Part 2: Mining and Filtering Implications

Once the context is clean, we extract the implications (association
rules).

``` r
# We use the original context for more results
fc$find_implications()
rules <- fc$implications

cat("Total rules found:", rules$cardinality(), "\n")
#> Total rules found: 10
```

### 2.1 Filtering by Metrics

You can use standard `dplyr` verbs to filter rules based on their
quality measures: `support`, `lhs_size` (number of attributes in the
premise), `rhs_size`, and `size`.

``` r
# Get strong rules (support > 0.2) that are not trivial (size > 2)
strong_rules <- rules %>% 
  filter(support > 0.2, size > 2) %>% 
  arrange(desc(support))

strong_rules$print()
#> Implication set with 3 implications.
#> Rule 1: {no_moon} -> {small, near}
#> Rule 2: {large} -> {far, moon}
#> Rule 3: {medium} -> {far, moon}
```

### 2.2 Semantic Filtering

Often, you are looking for rules that involve specific attributes (e.g.,
“What implies having a moon?”). `fcaR` provides special helper functions
available only inside
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html):

- `lhs("A")` / `lhs_has("A")`: The Left-Hand Side MUST contain “A”.
- `rhs("B")` / `rhs_has("B")`: The Right-Hand Side MUST contain “B”.
- `not_lhs("C")`: The LHS must NOT contain “C”.
- `lhs_any("A", "B")`: The LHS must contain either “A” or “B”.

``` r
# Find rules that imply 'moon'
moon_rules <- rules %>% 
  filter(rhs("moon"))

cat("Rules implying 'moon':\n")
#> Rules implying 'moon':
moon_rules$print()
#> Implication set with 3 implications.
#> Rule 1: {far} -> {moon}
#> Rule 2: {large} -> {far, moon}
#> Rule 3: {medium} -> {far, moon}
```

### 2.3 Complex Pipelines

You can combine metrics, semantic logic, sorting, and slicing in a
single pipeline. This allows for very specific queries like:

> \*“Find me the top 3 most supported rules about large planets that do
> not concern distance.”

``` r
specific_rules <- rules %>% 
  filter(
    lhs("large"),     # Must be about large planets
    not_lhs("far"),    # Ignore far planets
    support >= 0.2    # Minimum support threshold
  ) %>% 
  arrange(desc(support)) %>% 
  slice(1:3)          # Take the top 3

specific_rules$print()
#> Implication set with 1 implications.
#> Rule 1: {large} -> {far, moon}
```

## Conclusion

The integration of `dplyr` into `fcaR` allows for a fluid, readable, and
powerful workflow. You can clean your contexts and query your rule sets
using the same tidy syntax you use for standard data frames.
