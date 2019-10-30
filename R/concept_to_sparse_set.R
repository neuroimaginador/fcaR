.concept_to_sparse_set <- function(concept,
                                   objects,
                                   attributes) {

  extent <- sparse_set$new(attributes = objects,
                           M = concept[[1]])

  intent <- sparse_set$new(attributes = attributes,
                           M = concept[[2]])

  return(sparse_concept$new(extent, intent))

}
