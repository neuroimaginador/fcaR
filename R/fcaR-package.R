## usethis namespace: start
#' @useDynLib fcaR, .registration = TRUE
## usethis namespace: end
NULL

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL

#' fcaR: Tools for Formal Concept Analysis
#'
#' The aim of this package is to provide tools to perform  fuzzy formal concept analysis (FCA) from within R.
#' It provides functions to load and save a Formal Context, extract its concept lattice and implications.
#' In addition, one can use the implications to compute semantic closures of fuzzy sets and, thus, build recommendation systems.
#'
#'The fcaR package provides data structures which allow the user to work seamlessly with formal contexts and sets of implications. More explicitly, three main classes are implemented, using the \code{R6} object-oriented-programming paradigm in R:
#'
#' - \code{FormalContext} encapsulates the definition of a formal context \eqn{(G, M, I)}, being \eqn{G} the set of objects, \eqn{M} the set of attributes and \eqn{I} the (fuzzy) relationship matrix, and provides methods to operate on the context using FCA tools.
#' - \code{ImplicationSet} represents a set of implications over a specific formal context.
#' - \code{ConceptLattice} represents the set of concepts and their relationships, including methods to operate on the lattice.
#'
#' Two additional helper classes are implemented:
#' - \code{SparseSet} is a class solely used for visualization purposes, since it encapsulates in sparse format a (fuzzy) set.
#' - \code{SparseConcept} encapsulates internally both extent and intent of a formal concept as \code{SparseSet}.
#' Since fcaR is an extension of the data model in the arules package, most of the methods and classes implemented interoperates with the main \code{S4} classes in arules (\code{transactions} and \code{rules}).
#'
#' @examples
#' # Build a formal context
#' fc_planets <- FormalContext$new(planets)
#'
#' # Find its concepts and implications
#' fc_planets$find_implications()
#'
#' # Print the extracted implications
#' fc_planets$implications
#'
#' @references
#'
#' Guigues J, Duquenne V (1986). “Familles minimales d'implications informatives résultant d'un tableau de données binaires.” _Mathématiques et Sciences humaines_, *95*, 5-18.
#'
#' Ganter B, Wille R (1999). _Formal concept analysis : mathematical foundations_. Springer. ISBN 3540627715.
#'
#' Cordero P, Enciso M, Mora Á, Pérez de Guzman I (2002). “SLFD Logic: Elimination of Data Redundancy in Knowledge Representation.” _Advances in Artificial Intelligence -   IBERAMIA 2002_, *2527*, 141-150. doi:   10.1007/3-540-36131-6_15 (URL: http://doi.org/10.1007/3-540-36131-6_15).
#'
#' Belohlavek R (2002). “Algorithms for fuzzy concept lattices.” In _Proc. Fourth Int. Conf. on Recent Advances in Soft Computing_. Nottingham, United Kingdom, 200-205.
#'
#' Hahsler M, Grun B, Hornik K (2005). “arules - a computational environment for mining association rules and frequent item sets.” _J Stat Softw_, *14*, 1-25.
#'
#' Mora A, Cordero P, Enciso M, Fortes I, Aguilera G (2012). “Closure via functional dependence simplification.” _International Journal of Computer Mathematics_, *89*(4), 510-526.
#' Belohlavek R, Cordero P, Enciso M, Mora Á, Vychodil V (2016). “Automated prover for attribute dependencies in data with grades.” _International Journal of Approximate Reasoning_, *70*, 51-67.
#'
#' @docType package
#' @name fcaR
NULL
