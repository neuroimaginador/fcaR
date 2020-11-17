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
  - Added examples in the documentation.
  - Changed cat() to message() in functions not related to printing.
  - Added proper contributors.
* Updated vignettes

# fcaR 1.0.0

* Added a `NEWS.md` file to track changes to the package.
