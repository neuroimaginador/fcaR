.concept_to_SparseSet <- function(concept,
                                   objects,
                                   attributes) {

  extent <- SparseSet$new(attributes = objects,
                           M = concept[[1]])

  intent <- SparseSet$new(attributes = attributes,
                           M = concept[[2]])

  return(SparseConcept$new(extent, intent))

}
