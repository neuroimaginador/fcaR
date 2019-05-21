##%######################################################%##
#                                                          #
####                        Demo                        ####
####                         in                         ####
####         https://www.sciencedirect.com/sci          ####
####         ence/article/pii/S0957417415002869         ####
#                                                          #
##%######################################################%##


objects <- paste0(1:5)
n_objects <- length(objects)

attributes <- letters[1:6]
n_attributes <- length(attributes)

I <- matrix(data = c(1, 1, 1, 0, 1,
                     1, 0, 1, 1, 0,
                     0, 0, 1, 0, 0,
                     0, 0, 1, 1, 0,
                     0, 0, 0, 1, 0,
                     0, 0, 1, 1, 0),
            nrow = n_objects,
            byrow = FALSE)

grades_set <- sort(unique(as.vector(I)))

colnames(I) <- attributes
rownames(I) <- objects

print(I)

fc <- formal_context$new(I, grades_set)

fc$get_concepts()

fc$plot()

