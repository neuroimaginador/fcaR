## Adding equivalence rules

equivalencesRegistry$set_entry(
  method = "Composition",
  fun = .composition,
  description = "A -> B and A -> C equivalent to A -> BC"
)

equivalencesRegistry$set_entry(
  method = "Generalization",
  fun = .generalization,
  description = "A -> B and C -> D with C subset of A and B subset of D then remove A -> B"
)

equivalencesRegistry$set_entry(
  method = "Reduction",
  fun = .reduction,
  description = "A -> B equivalent to A -> B-A"
)

equivalencesRegistry$set_entry(
  method = "Simplification",
  fun = .simplification,
  description = "A -> B and C -> D with A, B disjoint, and A subset of C, equivalent to A -> B, C-B -> D-B"
)

equivalencesRegistry$set_entry(
  method = c("Right Simplification", "RSimplification"),
  fun = Rsimplification,
  description = "A -> B and C -> D  with A, B disjoint, and A subset of CD, equivalent to A -> B, C -> D-B"
)

equivalencesRegistry$set_entry(
  method = "Reorder",
  fun = reorder,
  description = "Reorder the implications according to the size of their LHS and RHS"
)

## Adding concept miners

conceptRegistry$set_entry(
  method = "NextClosure",
  fun = next_closure_concepts,
  description = "Original NextClosure method introduced by Ganter."
)

conceptRegistry$set_entry(
  method = "FastCbO",
  fun = FuzzyFCbO,
  description = "Adaptation of FastCbO go work on binary and fuzzy contexts, introduced in Lopez-Rodriguez et al. (2025)."
)

conceptRegistry$set_entry(
  method = "InClose",
  fun = InClose,
  description = "Adaptation of InClose go work on binary and fuzzy contexts, introduced in Lopez-Rodriguez et al. (2025)."
)


### CRAN hack for ggplot2
utils::globalVariables(c("fill_color", "label", "label_bottom", "label_top", "x", "y"))
