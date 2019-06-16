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

colnames(I) <- attributes
rownames(I) <- objects

print(I)

fc <- formal_context$new(I)

fc$extract_implications_concepts(verbose = TRUE)
fc$implications$length()

system.time(
  fc$implications$batch_apply(rules = c("composition",
                                        "generalization"),
                              batch_size = 25000L)
)
fc$implications$length()


fc$plot_lattice()
fc$plot_context()

fc$get_concept_support()
fc$get_implication_support()


##%######################################################%##
#                                                          #
####                    Thresholding                    ####
#                                                          #
##%######################################################%##

# This replicates the concept lattice from the above paper

I2 <- I * (I > 0.5)
I2[I2 > 0] <- 1

colnames(I2) <- attributes
rownames(I2) <- objects

print(I2)

fc2 <- formal_context$new(I)

fc2$extract_implications_concepts()

fc2$implications

system.time(
  fc2$implications$batch_apply(rules = c("composition",
                                        "generalization"),
                              batch_size = 25000L)
)
fc2$implications$length()

fc2$implications

fc2$plot_lattice()

fc$get_concept_support()
fc$get_implication_support()

