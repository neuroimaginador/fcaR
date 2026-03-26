# Experimental Block Relations and Bonds

#' @description
#' The Arrow-based Block Constructor (Experimental)
#'
#' @param fc A FormalContext object (should be standardized/reduced).
#'
#' @return A list of block relations (matrices).
#' @noRd
internal_compute_block_relations <- function(fc) {
  # 1. Ensure we are in the irreducible skeleton
  fc_std <- fc$get_core()
  # fcaR incidence() is (Objects x Attributes)
  I <- as.matrix(fc_std$incidence())
  
  # 2. Identify Critical Zeroes (Double Arrows)
  # get_arrow_relations() is (Attributes x Objects)
  arrows <- as.matrix(fc_std$get_arrow_relations())
  
  # which(arrows == 3) returns indices in (Attributes x Objects)
  critical_zeroes <- which(arrows == 3, arr.ind = TRUE)
  # row index is attribute index m
  # col index is object index g
  
  if (nrow(critical_zeroes) == 0) {
    return(list(t(I))) # Return in fcaR standard (Att x Obj)
  }
  
  # 3. Greedy Construction
  lista_j <- list()
  remaining_zeroes <- seq_len(nrow(critical_zeroes))
  
  # Cache I_oa for the C++ closure
  I_oa <- Matrix::as.matrix(Matrix::t(fc_std$I))
  
  while(length(remaining_zeroes) > 0) {
    # Pivot zero
    idx_p <- remaining_zeroes[1]
    g_p <- critical_zeroes[idx_p, "row"]
    m_p <- critical_zeroes[idx_p, "col"]
    
    # Initialize J as I (Objects x Attributes)
    J <- I
    
    # Try to fill other critical zeroes
    for (idx_i in seq_len(nrow(critical_zeroes))) {
      # Skip if it is the pivot zero
      if (idx_i == idx_p) next
      
      g_i <- critical_zeroes[idx_i, "row"]
      m_i <- critical_zeroes[idx_i, "col"]
      
      # Skip if already filled
      if (J[g_i, m_i] == 1) next
      
      # Try filling (g_i, m_i)
      J_temp <- J
      J_temp[g_i, m_i] <- 1
      # Optimized C++ closure
      J_temp <- bonds_closure_cpp(as.matrix(J_temp), I_oa)
      
      # If pivot zero remains 0, accept change
      if (J_temp[g_p, m_p] == 0) {
        J <- J_temp
      }
    }
    
    # Add to list (same as incidence(): Obj x Att)
    lista_j[[length(lista_j) + 1]] <- J
    
    # Update remaining zeroes
    covered_in_this_run <- c()
    for (idx in remaining_zeroes) {
      gr <- critical_zeroes[idx, "row"]
      mr <- critical_zeroes[idx, "col"]
      if (J[gr, mr] == 0) {
        covered_in_this_run <- c(covered_in_this_run, idx)
      }
    }
    
    remaining_zeroes <- setdiff(remaining_zeroes, covered_in_this_run)
  }
  
  return(lista_j)
}

#' @description
#' Closure of a binary relation to become a bond.
#' Now optimized in C++.
#'
#' @param J Incidence matrix (Objects x Attributes).
#' @param fc FormalContext (standardized).
#' @noRd
internal_operator_c_block <- function(J, fc) {
  # fc$I is (Attributes x Objects)
  I_oa <- Matrix::as.matrix(Matrix::t(fc$I))
  res <- bonds_closure_cpp(as.matrix(J), I_oa)
  return(res)
}
