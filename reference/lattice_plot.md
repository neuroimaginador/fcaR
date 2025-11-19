# Plot Concept Lattice

Visualization of the concept lattice using 'ggraph'.

## Usage

``` r
lattice_plot(
  nodes_df,
  cover_matrix,
  method = "sugiyama",
  mode = NULL,
  objects = NULL,
  attributes = NULL,
  object_names = TRUE,
  to_latex = FALSE,
  extents = NULL,
  intents = NULL,
  ...
)
```

## Arguments

- nodes_df:

  Data frame with 'id'.

- cover_matrix:

  Sparse matrix.

- method:

  Layout method ("sugiyama", "force").

- mode:

  Labeling mode ("reduced", "full", "empty").

- objects:

  Character vector.

- attributes:

  Character vector.

- object_names:

  Logical (Deprecated).

- to_latex:

  Logical.

- extents:

  List of extents.

- intents:

  List of intents.

- ...:

  Extra args.
