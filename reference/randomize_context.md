# Randomize an Existing Formal Context

Modifies the incidence matrix of a formal context to create a random
variation while preserving certain statistical properties. This is
essential for statistical significance testing in FCA (e.g., "is this
concept structure random?").

## Usage

``` r
randomize_context(fc, method = "swap", iterations = NULL)
```

## Arguments

- fc:

  (`FormalContext`) The context to randomize.

- method:

  (character) The randomization strategy:

  - `"swap"`: Edge Swapping (Curveball algorithm). Preserves exact row
    sums and column sums (marginal distributions). The structure
    changes, but the statistics of objects and attributes remain
    identical.

  - `"rewire"`: Edge Rewiring. Preserves only the global density (total
    number of 1s). Row and column sums may change.

- iterations:

  (integer) Number of swap/rewire operations to perform. Default is
  `10 * number of 1s`, which is usually sufficient for mixing.

## Value

A new `FormalContext` object with the randomized incidence.

## Examples

``` r
data(planets)
fc <- FormalContext$new(planets)

# 1. Edge Swapping (Preserves degree distribution)
# Useful for null-model testing
fc_rand_swap <- randomize_context(fc, method = "swap")

# Verify marginals are preserved
colSums(fc$incidence())
#>   small  medium   large    near     far    moon no_moon 
#>       5       2       2       4       5       7       2 
colSums(fc_rand_swap$incidence())
#>   small  medium   large    near     far    moon no_moon 
#>       5       2       2       4       5       7       2 

# 2. Rewiring (Preserves only density)
fc_rand_rewire <- randomize_context(fc, method = "rewire")
```
