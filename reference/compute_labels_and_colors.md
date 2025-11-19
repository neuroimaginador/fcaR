# Compute Labels and Colors for Lattice Nodes

Internal function to calculate node labels based on the selected mode.

## Usage

``` r
compute_labels_and_colors(
  nodes_df,
  cover_edges,
  extents,
  intents,
  obj_names,
  att_names,
  mode
)
```

## Arguments

- nodes_df:

  Data frame with 'id'.

- cover_edges:

  Data frame with 'from', 'to'.

- extents:

  List of numeric vectors.

- intents:

  List of numeric vectors.

- obj_names:

  Character vector.

- att_names:

  Character vector.

- mode:

  Character: "full", "attributes", "reduced", "empty".

## Value

A data frame extending nodes_df with labels and colors.
