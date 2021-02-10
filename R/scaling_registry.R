## setup registry for conceptual scaling
#' Scaling Registry
#'
#' @details
#' This is a registry that stores the implemented scales that can be
#' applied using the \code{scale()} method in an
#' \code{FormalContext}.
#'
#' One can obtain the list of available equivalence operators by:
#' \code{scalingRegistry$get_entry_names()}
#'
#' @export
#' @import registry
scalingRegistry <- registry::registry(registry_class = "scaling_registry",
                                      entry_class = "scale")

scalingRegistry$set_field("method",
                          type = "character",
                          is_key = TRUE,
                          index_FUN = registry::match_partial_ignorecase)

scalingRegistry$set_field("fun",
                          type = "function",
                          is_key = FALSE)
scalingRegistry$set_field("description",
                          type = "character",
                          is_key = FALSE)

##%######################################################%##
#                                                          #
####                       Scales                       ####
#                                                          #
##%######################################################%##

scalingRegistry$set_entry(method = "Nominal",
                          fun = nominal_scaling,
                          description = "Nominal scaling")

scalingRegistry$set_entry(method = "Ordinal",
                          fun = ordinal_scaling,
                          description = "Ordinal scaling")

scalingRegistry$set_entry(method = "Interordinal",
                          fun = interordinal_scaling,
                          description = "Interordinal scaling")

scalingRegistry$set_entry(method = "Biordinal",
                          fun = biordinal_scaling,
                          description = "Biordinal scaling")

scalingRegistry$set_entry(method = "Interval",
                          fun = interval_scaling,
                          description = "Interval scaling")


