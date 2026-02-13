# fcaR 1.5.0

Improvements:

* **OOP Refactoring:** `ImplicationSet` now inherits from `RuleSet`, following proper object-oriented design. Shared methods (filtering, subsetting, printing, serialization, etc.) live in the parent `RuleSet` class, eliminating ~600 lines of duplicated code. `ImplicationSet` retains only FCA-specific methods (`closure()`, `apply_rules()`, `to_basis()`, `to_direct_optimal()`, etc.).
* **Tidyverse for RuleSet:** The `dplyr` verbs (`filter()`, `arrange()`, `slice()`) now work on `RuleSet` objects in addition to `ImplicationSet`, via S3 method dispatch.

# fcaR 1.4.1

* **JSON Import/Export:** Added `to_json()` methods and corresponding `*_from_json()` functions for `FormalContext`, `ConceptLattice`, `ImplicationSet`, and `RuleSet`. This allows for efficient serialization of all major data structures in `fcaR`, including recursive export of nested objects.

# fcaR 1.4.0

Major Enhancements:

* **Tidyverse integration:** Implemented S3 methods to support `dplyr` verbs, allowing for a fluent, grammar-based manipulation of FCA objects:
  * **FormalContext:** Support for `select()` (attributes), `filter()` (objects), `mutate()` (feature engineering), `arrange()` (sorting), and `rename()`. Includes support for `tidyselect` helpers (e.g., `starts_with()`).
  * **ImplicationSet:** Support for `filter()` (based on metrics or attributes), `arrange()` (sorting rules), and `slice()` (subsetting by index).
* **Semantic Rule Filtering:** Introduced helper functions for `ImplicationSet` filtering to query rules based on attribute presence/absence: `lhs()`, `rhs()`, `not_lhs()`, `lhs_any()`, etc. This allows querying rules like `filter(rhs("Attribute_A"), support > 0.2)`.
* **Mining Causal Association Rules:** Implemented the `find_causal_rules()` method in `FormalContext`, enabling the discovery of causal rules by controlling for confounding variables using the "Fair Odds Ratio" on matched pairs.

Improvements:

* **Robust Subsetting:** Completely rewritten `subcontext()` method in `FormalContext`. It now robustly handles negative indices, logical vectors, and character vectors, and prevents dimension collapsing issues (using `drop = FALSE`) that previously caused errors with the `Matrix` package.
* **Metadata Preservation:** Rewritten `[` and related methods of `ImplicationSet`. These ensure that critical context metadata (such as the number of objects $N$ for support calculation) is preserved when filtering or sorting rules, fixing previous issues where metadata was lost.
* **Data Safety:** Enhanced type safety in internal functions to strictly handle integer indices, preventing errors with `dplyr` attributes.

Documentation:

* **New Vignette:** Added `fcaR_dplyr` vignette illustrating the new data manipulation workflow.
* **New Vignette:** Added `causal` vignette explaining the new causal mining functionality and its application to Simpson's Paradox.

Fixes:

* Fixed `Matrix` coercion errors (`dgCMatrix` to `data.frame`) in R 4.x when using internal incidence matrices.
* Fixed `fixupDN.if.valid` errors from the `Matrix` package when filtering operations resulted in empty contexts (0 objects or 0 attributes).
* Resolved floating-point precision issues in unit tests when comparing support values.
* Reduced dependencies: moved `ggplot2`, `ggraph`, `igraph`, `rstudioapi`, and `yaml` to Suggests.
* Removed `forcats` and `magrittr` dependencies by using base R equivalents and the native pipe `|>`.

# fcaR 1.3.1

* Added tests

# fcaR 1.3.0

Major Enhancements:

* **Matrix factorization:** Added `factorize()` method to `FormalContext` class. It now implements two state-of-the-art algorithms:
  * **GreConD+**: For Boolean and Fuzzy matrix factorization with grades and overcovering (Belohlavek & Trneckova, 2024). It fully supports custom fuzzy logics set in the context.
  * **ASSO**: A heuristic algorithm based on association rules for Boolean matrix factorization.
* **Advanced randomization:** New suite of functions to generate and perturb datasets for statistical testing:
  * `RandomContext()`: Generates synthetic contexts using **Uniform** or **Dirichlet** distributions (mimicking real-world data structure).
  * `randomize_context()`: Randomizes existing contexts via **Edge swapping** (preserves marginal sums) or **Rewiring** (preserves density).
  * **Distributive Generators:** Added `RandomDistributiveContext()` to generate synthetic data guaranteed to produce distributive lattices (based on Birkhoff's theorem).
* **Robustness & performance:** The **InClose** algorithm has been completely refactored to use the C-API directly (avoiding Rcpp overhead). This  significantly improves performance for sparse matrices.

New Functionality:

* **Advanced metrics:** Added methods `stability()`, `separation()`, and `fuzzy_density()` to `ConceptLattice` to compute concept quality metrics.
* **Lattice properties:** Added methods to `ConceptLattice` to efficiently check algebraic properties using sparse matrix operations: `is_distributive()`, `is_modular()`, `is_semimodular()`, and `is_atomic()`.
* **Fuzzy algorithms:** In fuzzy contexts, the user can now select the algorithm for `find_concepts()`: "InClose" (default), "FastCbO", or "NextClosure".
* **Implications:**
  * Added `to_direct_optimal()` to convert implication sets into the direct optimal basis.
  * Added `use_hedge()` and `get_hedge()` to manage hedges in fuzzy implication closures.
* **Repo integration:** The connection to fcarepository.org is now managed more elegantly, including an RStudio addin to fetch contexts.

Improvements:

* **Visualization:** Removed dependency on `hasseDiagram`. Implemented a new native graphics engine for concept lattices.
* **Efficiency:** Optimized C functions for concept support computation and general lattice mining. Also, the NextClosure implementation for building the Duquenne-Guigues basis of implications in the binary case has been optimized.
* **Documentation:** Added new vignettes: `advanced_lattice_metrics`, `creating_contexts`, `fuzzy_fca`, `lattice_visualization`, `matrix_factorization`, `random_contexts` and `lattice_properties`.

# fcaR 1.2.2

Enhancements:

* Added more unit tests.
* Minor changes to the plotting of formal contexts.
* Now the `fc$scale()` function admits a new argument `bg` (default: FALSE) which, if set to TRUE, avoids computing the background knowledge of the scales.

Fixes:

* Changed the package documentation format to comply with the new roxygen2 _PACKAGE sentinel.

# fcaR 1.2.1

Enhancements:

* Other logics have been implemented. Now, we can use `fc$use_logic()` to select one of the `available_logics()`.
* Improved export to LaTeX.

Bugfixes:

* Fixes required by the new version of Matrix and the new use of HTML Tidy in R 4.2.
* Some rounding errors might induce errors in the computations. These has been fixed.

# fcaR 1.2.0

* Fixes required by the new version of Matrix and the new use of HTML Tidy in R 4.2.

# fcaR 1.1.1

Enhancements:

* The user can control the number of decimal digits when exporting to LaTeX or when printing formal contexts, concept lattices and implications. Just use fcaR_options(decimal_places = n), where n is the number of desired decimal digits.

New functionality:

* Now the package uses the _settings_ package to manage several options. Currently, the only option is the number of decimal digits to use when printing or exporting to LaTeX.

Bugfixes:

* Fixed exporting to latex with special characters such as $, _, etc.

# fcaR 1.1.0

Enhancements:

* Better printing of Sets.
* More optimized ConceptLattice class. Now it inherits from a ConceptSet superclass with generic functions.

New functionality:

* Added function to compute the **difference** of two Sets.
* Added function to compute the **dual** of a FormalContext.
* Now one can create a FormalContext from a CSV, CXT or RDS file directly, without needing to "load()" it.
* FormalContexts can now be saved to CXT format, in addition to RDS.
* Added functions to compute the top and the bottom of a concept lattice.
* Added new function sub() to extract a single Concept from a ConceptSet.
* Added functions %holds_in% and %respects%, which check the **validity** of a set of implications in a formal context, and if a list of attribute sets respect an implication set.
* Added functions %entails% and %~% to check the **entailment** and **equivalence** between two implication sets.
* Added new convenience function to map attributes between Sets, so computing intents, extents and closures is more robust.
* Added new functions `%&%` and `%|%` that compute the intersection (logical _and_) and the union (_or_ operation) on Sets.
* **Conceptual scaling**, including nominal, ordinal, interordinal, biordinal and interval scales for many-valued formal contexts. Also, computation of background knowledge from the applied scales and of the implications that hold in the formal context. Added new vignette.

_Breaking changes_:

* The former SparseSet and SparseConcept classes are now named Set and Concept. Thus, to create an object of these types, just use Set\$new(...) or Concept\$new(...). Analogously, the former function as_SparseSet() is now as_Set().

Bugfixes:

* Minor bugfixes in several functions.

# fcaR 1.0.7

Bugfixes:

* Minor bugfixes in several functions.
* Fixed C source that could generate some problems.

Enhancements:

* Reduced number of dependencies.
* Better printing of FormalContexts, specially for binary FormalContexts.

# fcaR 1.0.6

Bugfixes:

* Detect if R has been built with no long double capabilities and warn the user if she tries to plot FormalContexts and ConceptLattices.

# fcaR 1.0.5

Bugfixes:

* Fixes #17. The reduced set of implications after performing closure with reduce == TRUE now stores the matrix I, so it can be re-exported to the arules format.

Enhancements:

* More efficient version of the simplification rule. Now it performs linearly on the number of implications.
* Added a way to extend the equivalence rules by means of the registry package. This has been used to introduce the "Right Simplification" and the "Reorder" rules.
* New vignette to show how to extend equivalence rules in the package.
* More efficient version of the NextClosure algorithm to mine concepts and implications.

# fcaR 1.0.4

Bugfixes:

* Fixes #15. A problem with version 1.6-6 of arules.
* Bugfix when using the trivial context (S, S, !=)
* Several bugfixes in LaTeX output.

Enhancements:

* Added export of plots to TiKZ to be included in LaTeX documents.
* Faster computation of concepts and implications.
* Added new function to decompose a concept in its irreducible components.
* Added function to compute the canonical basis from an ImplicationSet.

# fcaR 1.0.3

* Changes to some C functions since they could potentially give a segfault.

# fcaR 1.0.2

* Made minor changes in the DESCRIPTION.

# fcaR 1.0.1

* Made changes suggested by CRAN:

  * Added examples in the documentation.
  * Changed cat() to message() in functions not related to printing.
  * Added proper contributors.
  
* Updated vignettes

# fcaR 1.0.0

* Added a `NEWS.md` file to track changes to the package.
