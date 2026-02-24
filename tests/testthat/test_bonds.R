test_that("Bonds Computation Works Correspondingly", {
  
  # Create two small formal contexts
  mat1 <- matrix(c(1, 0, 1, 1, 
                   1, 1, 0, 0, 
                   0, 1, 1, 0), nrow = 3, byrow = TRUE)
  rownames(mat1) <- c("O1", "O2", "O3")
  colnames(mat1) <- c("A1", "A2", "A3", "A4")
  fc1 <- FormalContext$new(mat1)
  
  mat2 <- matrix(c(0, 1, 1,
                   1, 0, 1,
                   1, 1, 0), nrow = 3, byrow = TRUE)
  rownames(mat2) <- c("P1", "P2", "P3")
  colnames(mat2) <- c("B1", "B2", "B3")
  fc2 <- FormalContext$new(mat2)
  
  # Return lattice
  bonds_lattice <- bonds(fc1, fc2)
  expect_s3_class(bonds_lattice, "BondLattice")
  expect_s3_class(bonds_lattice, "ConceptLattice")
  
  # Calculate how many concepts (bonds) we have
  n_bonds <- bonds_lattice$size()
  expect_gt(n_bonds, 0)
  
  # Test returning formal contexts for the bonds
  bonds_list <- bonds_lattice$get_bonds()
  expect_type(bonds_list, "list")
  expect_equal(length(bonds_list), n_bonds)
  
  # Pick a random bond formal context
  bond_fc <- bonds_list[[ceiling(n_bonds / 2)]]
  
  expect_s3_class(bond_fc, "FormalContext")
  expect_equal(length(bond_fc$objects), length(fc1$objects))
  expect_equal(length(bond_fc$attributes), length(fc2$attributes))
  
  # Verify that for the bond, rows are intents of fc2 and columns are extents of fc1 
  
  bond_mat <- bond_fc$incidence()
  
  for (i in seq_len(nrow(bond_mat))) {
    row_vec <- bond_mat[i, ]
    
    # an intent of K_2 is a closed set in K_2 attributes
    S <- Set$new(attributes = fc2$attributes)
    if (sum(row_vec) > 0) {
      S$assign(attributes = names(row_vec)[which(row_vec > 0)], values = 1)
    }
    
    expect_true(fc2$is_closed(S))
  }
  
  # Any column is an extent of K_1. An extent of K_1 is a closed set in K_1^d
  fc1d <- fc1$dual()
  for (j in seq_len(ncol(bond_mat))) {
    col_vec <- bond_mat[, j]
    
    S <- Set$new(attributes = fc1d$attributes) # which is G1
    if (sum(col_vec) > 0) {
      S$assign(attributes = names(col_vec)[which(col_vec > 0)], values = 1)
    }
    
    expect_true(fc1d$is_closed(S))
  }
})
