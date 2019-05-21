##%######################################################%##
#                                                          #
####                     Demo based                     ####
####                         on                         ####
####           https://ieeexplore.ieee.org/st           ####
####           amp/stamp.jsp?arnumber=7792568           ####
#                                                          #
##%######################################################%##

objects <- paste0("S", 1:4)
n_objects <- length(objects)

attributes <- letters[1:7]
n_attributes <- length(attributes)

I <- matrix(data = c(0.1, 0.1, 0.8, 0.7,
                     0.1, 0.7, 0.1, 0.2,
                     0.8, 0.2, 0.1, 0.1,
                     0.2, 0.1, 0.8, 0.7,
                     0.8, 0.9, 0.2, 0.3,
                     0.2, 0.2, 1.0, 0.8,
                     0.8, 0.8, 0.0, 0.2),
            nrow = n_objects,
            byrow = FALSE)

grades_set <- sort(unique(as.vector(I)))

colnames(I) <- attributes
rownames(I) <- objects

print(I)

fc <- formal_context$new(I, grades_set)

fc$get_concepts()

fc$plot()

##%######################################################%##
#                                                          #
####                    Thresholding                    ####
#                                                          #
##%######################################################%##

# This replicates the concept lattice from the above paper

I2 <- I * (I > 0.5)
I2[I2 > 0] <- 1

grades_set <- sort(unique(as.vector(I2)))

colnames(I2) <- attributes
rownames(I2) <- objects

print(I2)

fc2 <- formal_context$new(I2, grades_set)

fc2$get_concepts()

fc2$plot()
