## setup registry for equivalences rules
#' Equivalence Rules Registry
#'
#' @details
#' This is a registry that stores the equivalence rules that can be
#' applied using the \code{apply_rules()} method in an
#' \code{ImplicationSet}.
#'
#' One can obtain the list of available equivalence operators by:
#' \code{equivalencesRegistry$get_entry_names()}
#'
#' @export
#' @import registry
equivalencesRegistry <- registry::registry(registry_class = "equivalence_registry",
                                           entry_class = "equivalence_rule")

equivalencesRegistry$set_field("method",
                               type = "character",
                               is_key = TRUE,
                               index_FUN = registry::match_partial_ignorecase)

equivalencesRegistry$set_field("fun",
                               type = "function",
                               is_key = FALSE)
equivalencesRegistry$set_field("description",
                               type = "character",
                               is_key = FALSE)
