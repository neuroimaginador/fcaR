##%######################################################%##
#                                                          #
####                 Demo for fuzzyFCA                  ####
#                                                          #
##%######################################################%##

objects <- paste0("O", 1:10)
n_objects <- length(objects)

attributes <- paste0("A", 1:5)
n_attributes <- length(attributes)

##%######################################################%##
#                                                          #
####                       Crisp                        ####
#                                                          #
##%######################################################%##

grades_set <- c(0, 1)

I <- sample(grades_set,
            size = n_objects * n_attributes,
            replace = TRUE)

I <- matrix(I, nrow = n_objects)
colnames(I) <- attributes
rownames(I) <- objects

print(I)

fc <- formal_context$new(I, grades_set)

fc$get_concepts()

fc$plot()


##%######################################################%##
#                                                          #
####                       Fuzzy                        ####
#                                                          #
##%######################################################%##

grades_set <- seq(0, 1, by = 0.1)

I <- sample(grades_set,
            size = n_objects * n_attributes,
            replace = TRUE)

I <- matrix(I, nrow = n_objects)
colnames(I) <- attributes
rownames(I) <- objects

fc <- formal_context$new(I, grades_set)

fc$get_concepts()

fc$plot()
