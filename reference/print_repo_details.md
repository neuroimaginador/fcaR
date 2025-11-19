# Print Details of Repository Contexts

Prints a formatted summary of the contexts available in the FCA
Repository to the console. It displays the filename, title, dimensions
(objects x attributes), and description for each entry.

## Usage

``` r
print_repo_details(meta)
```

## Arguments

- meta:

  A list of metadata objects, typically obtained via
  [`get_fcarepository_contexts`](https://neuroimaginador.github.io/fcaR/reference/get_fcarepository_contexts.md).

## Value

Prints the summary to the console. Returns `NULL` invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
meta <- get_fcarepository_contexts()
print_repo_details(meta)
} # }
```
