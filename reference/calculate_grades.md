# Calculate Concept Grades (Levels)

Calculates the grade (level) of each concept using the longest path from
the bottom element. This is a fast C++ implementation.

## Usage

``` r
calculate_grades(concept_ids, edge_from, edge_to)
```

## Arguments

- concept_ids:

  A vector of concept IDs (integers).

- edge_from:

  A vector of source concept IDs from the cover relation (Hasse
  diagram).

- edge_to:

  A vector of target concept IDs from the cover relation (Hasse
  diagram).

## Value

An integer vector of the calculated grade for each concept ID.
