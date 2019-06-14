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

colnames(I) <- attributes
rownames(I) <- objects

print(I)

fc <- formal_context$new(I)

fc$extract_implications_concepts()
fc$implications$length()

fc$implications$batch_apply(rules = c("composition",
                                      "generalization"))
fc$implications$length()

fc$plot_context()

fc$plot_lattice()

