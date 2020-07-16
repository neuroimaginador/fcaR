library("registry")
## setup registry for equivalences rules
#' @importFrom registry registry match_partial_ignorecase
#' @export
equivalencesRegistry <- registry(registry_class = "equivalence_registry",
                                entry_class = "equivalence_rule")

equivalencesRegistry$set_field("method",
                              type = "character",
                              is_key = TRUE,
                              index_FUN = match_partial_ignorecase)

equivalencesRegistry$set_field("fun",
                              type = "function",
                              is_key = FALSE)
equivalencesRegistry$set_field("description",
                              type = "character",
                              is_key = FALSE)
