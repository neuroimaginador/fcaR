# Fetch a Formal Context from the FCA Repository

Downloads a context file from the fcarepository.org (via GitHub mirror),
parses it, and returns a FormalContext object.

## Usage

``` r
fetch_context(filename, verbose = TRUE)
```

## Arguments

- filename:

  Character string. The ID/filename of the context (e.g.,
  "animals_en.cxt").

- verbose:

  Logical. If TRUE, prints metadata and progress messages using
  cli/glue.

## Value

A FormalContext object.
