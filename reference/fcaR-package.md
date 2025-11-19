# fcaR: Tools for Formal Concept Analysis

The aim of this package is to provide tools to perform fuzzy formal
concept analysis (FCA) from within R. It provides functions to load and
save a Formal Context, extract its concept lattice and implications. In
addition, one can use the implications to compute semantic closures of
fuzzy sets and, thus, build recommendation systems.

## Details

The fcaR package provides data structures which allow the user to work
seamlessly with formal contexts and sets of implications. More
explicitly, three main classes are implemented, using the `R6`
object-oriented-programming paradigm in R:

- `FormalContext` encapsulates the definition of a formal context \\(G,
  M, I)\\, being \\G\\ the set of objects, \\M\\ the set of attributes
  and \\I\\ the (fuzzy) relationship matrix, and provides methods to
  operate on the context using FCA tools.

- `ImplicationSet` represents a set of implications over a specific
  formal context.

- `ConceptLattice` represents the set of concepts and their
  relationships, including methods to operate on the lattice.

Two additional helper classes are implemented:

- `Set` is a class solely used for visualization purposes, since it
  encapsulates in sparse format a (fuzzy) set.

- `Concept` encapsulates internally both extent and intent of a formal
  concept as `Set`. Since fcaR is an extension of the data model in the
  arules package, most of the methods and classes implemented
  interoperates with the main `S4` classes in arules (`transactions` and
  `rules`).

## References

Guigues J, Duquenne V (1986). “Familles minimales d'implications
informatives résultant d'un tableau de données binaires.” *Mathématiques
et Sciences humaines*, *95*, 5-18.

Ganter B, Wille R (1999). *Formal concept analysis : mathematical
foundations*. Springer. ISBN 3540627715.

Cordero P, Enciso M, Mora Á, Pérez de Guzman I (2002). “SLFD Logic:
Elimination of Data Redundancy in Knowledge Representation.” *Advances
in Artificial Intelligence - IBERAMIA 2002*, *2527*, 141-150. doi:
10.1007/3-540-36131-6_15 (URL: http://doi.org/10.1007/3-540-36131-6_15).

Belohlavek R (2002). “Algorithms for fuzzy concept lattices.” In *Proc.
Fourth Int. Conf. on Recent Advances in Soft Computing*. Nottingham,
United Kingdom, 200-205.

Hahsler M, Grun B, Hornik K (2005). “arules - a computational
environment for mining association rules and frequent item sets.” *J
Stat Softw*, *14*, 1-25.

Mora A, Cordero P, Enciso M, Fortes I, Aguilera G (2012). “Closure via
functional dependence simplification.” *International Journal of
Computer Mathematics*, *89*(4), 510-526. Belohlavek R, Cordero P, Enciso
M, Mora Á, Vychodil V (2016). “Automated prover for attribute
dependencies in data with grades.” *International Journal of Approximate
Reasoning*, *70*, 51-67.

## See also

Useful links:

- <https://github.com/Malaga-FCA-group/fcaR>

- <https://neuroimaginador.github.io/fcaR/>

- Report bugs at <https://github.com/Malaga-FCA-group/fcaR/issues>

## Author

**Maintainer**: Domingo Lopez Rodriguez <dominlopez@uma.es>
([ORCID](https://orcid.org/0000-0002-0172-1585))

Authors:

- Angel Mora <amorabonilla@gmail.com>

- Jesus Dominguez

- Ana Villalon

## Examples

``` r
# Build a formal context
fc_planets <- FormalContext$new(planets)

# Find its concepts and implications
fc_planets$find_implications()

# Print the extracted implications
fc_planets$implications
#> Implication set with 10 implications.
#> Rule 1: {no_moon} -> {small, near}
#> Rule 2: {far} -> {moon}
#> Rule 3: {near} -> {small}
#> Rule 4: {large} -> {far, moon}
#> Rule 5: {medium} -> {far, moon}
#> Rule 6: {medium, large, far, moon} -> {small, near, no_moon}
#> Rule 7: {small, near, moon, no_moon} -> {medium, large, far}
#> Rule 8: {small, near, far, moon} -> {medium, large, no_moon}
#> Rule 9: {small, large, far, moon} -> {medium, near, no_moon}
#> Rule 10: {small, medium, far, moon} -> {large, near, no_moon}
```
