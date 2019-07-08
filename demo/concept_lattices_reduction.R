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
fc$implications$cardinality()

fc$implications$batch_apply(rules = c("composition",
                                      "generalization"))
fc$implications$length()

fc$plot_context()

fc$plot_lattice()

A1 <- c("c","d")
A11 <- to_sparse(A1,fc$attributes)

cl <- fc$implications$compute_closure(A11,reduce = TRUE)
cl
#cl1 <- from.sparse(cl)

cl2 <- fc$implications$compute_closure2(A11)
cl2
cl2$implications

Imp <- implication_set$new(attributes = fc$attributes,
                           lhs=cl$implications$lhs,rhs=cl$implications$rhs)

