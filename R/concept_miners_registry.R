#' Concept Miners Registry
#'
#' @details
#' This is a registry that stores the concept miners that can be
#' applied using the \code{find_concepts()} method in an
#' \code{FormalConcept}.
#'
#' One can obtain the list of available equivalence operators by:
#' \code{conceptRegistry$get_entry_names()}
#'
#' @export
#' @import registry
conceptRegistry <- registry::registry(
  registry_class = "concept_miner_registry",
  entry_class = "concept_miner")

conceptRegistry$set_field("method",
                               type = "character",
                               is_key = TRUE,
                               index_FUN = registry::match_partial_ignorecase)

conceptRegistry$set_field("fun",
                               type = "function",
                               is_key = FALSE)
conceptRegistry$set_field("description",
                               type = "character",
                               is_key = FALSE)
