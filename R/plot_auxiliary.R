#' @importFrom Matrix summary
sparse_matrix_to_edges <- function(sparse_matrix) {
  triplet <- Matrix::summary(sparse_matrix)

  # Manejo robusto de pesos (matrices ngCMatrix vs dgCMatrix)
  weights <- if ("x" %in% names(triplet)) triplet$x else rep(1, nrow(triplet))

  edges_df <- data.frame(
    from = as.integer(triplet$i),
    to = as.integer(triplet$j),
    weight = weights
  )

  # 1. Filtrar ceros
  edges_df <- edges_df[edges_df$weight != 0, ]

  # 2. CRÍTICO: Eliminar autociclos (la diagonal) para evitar la "Bomba"
  # El algoritmo de grado asume un grafo acíclico estricto (DAG).
  edges_df <- edges_df[edges_df$from != edges_df$to, ]

  return(edges_df[, c("from", "to")])
}

#' @title Calculate Concept Grades (Levels)
#' @description Calculates the grade (level) of each concept using the longest path
#'   from the bottom element. This is a fast C++ implementation.
#'
#' @param concept_ids A vector of concept IDs (integers).
#' @param edge_from A vector of source concept IDs from the cover relation (Hasse diagram).
#' @param edge_to A vector of target concept IDs from the cover relation (Hasse diagram).
#'
#' @return An integer vector of the calculated grade for each concept ID.
#' @useDynLib fcaR, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @export
calculate_grades <- function(concept_ids, edge_from, edge_to) {
  # Call the Rcpp function
  # The C++ function expects the concept IDs as well, to map output grades correctly.
  calculated_grades <- calculate_grades_rcpp(
    concept_ids = as.integer(concept_ids),
    edge_from = as.integer(edge_from),
    edge_to = as.integer(edge_to)
  )

  # The output vector is ordered according to concept_ids input
  names(calculated_grades) <- concept_ids
  return(calculated_grades)
}
