# Export Layout to TikZ (LaTeX)

Generates TikZ code for the concept lattice.

## Usage

``` r
export_to_tikz(
  plot_data,
  edges_df,
  width = 12,
  height = 12,
  standalone = FALSE,
  caption = NULL,
  ...
)
```

## Arguments

- plot_data:

  Data frame with node info.

- edges_df:

  Data frame with edge info.

- width:

  Numeric. Target width in cm (default: 12).

- height:

  Numeric. Target height in cm (default: 12).

- standalone:

  Logical. If TRUE, wraps code in a documentclass to be compiled
  directly.

- caption:

  Character. Optional caption for the figure.

- ...:

  Additional arguments (currently unused).

## Value

An object of class 'tikz_code'.
