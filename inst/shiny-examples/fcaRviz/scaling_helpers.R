# scaling_helpers.R
# Core functions to perform Formal Conceptual Scaling on multivalued datasets

#' Scale a nominal (categorical) column
#' @param df Data frame
#' @param col Column name
#' @return A matrix of binary columns
scale_nominal <- function(df, col) {
  values <- as.character(df[[col]])
  unique_vals <- sort(unique(values[!is.na(values)]))
  
  if (length(unique_vals) == 0) return(matrix(0, nrow = nrow(df), ncol = 0))
  
  mat <- matrix(0, nrow = nrow(df), ncol = length(unique_vals))
  colnames(mat) <- paste0(col, " = ", unique_vals)
  
  for (i in seq_along(unique_vals)) {
    val <- unique_vals[i]
    mat[which(values == val), i] <- 1
  }
  return(mat)
}

#' Scale an ordinal column (numerical/ordered values)
#' @param df Data frame
#' @param col Column name
#' @param thresholds Numeric vector of cuts. If NULL, uses sorted unique values.
#' @param direction Character, "le" (<=) or "ge" (>=) or "both"
#' @return A matrix of binary columns
scale_ordinal <- function(df, col, thresholds = NULL, direction = "le") {
  vals <- as.numeric(df[[col]])
  
  if (is.null(thresholds)) {
    thresholds <- sort(unique(vals[!is.na(vals)]))
  } else {
    thresholds <- sort(as.numeric(thresholds))
  }
  
  if (length(thresholds) == 0) return(matrix(0, nrow = nrow(df), ncol = 0))
  
  cols_list <- list()
  
  if (direction %in% c("le", "both")) {
    for (t in thresholds) {
      col_name <- paste0(col, " <= ", t)
      binary_vector <- rep(0, nrow(df))
      binary_vector[which(vals <= t)] <- 1
      cols_list[[col_name]] <- binary_vector
    }
  }
  
  if (direction %in% c("ge", "both")) {
    for (t in thresholds) {
      col_name <- paste0(col, " >= ", t)
      binary_vector <- rep(0, nrow(df))
      binary_vector[which(vals >= t)] <- 1
      cols_list[[col_name]] <- binary_vector
    }
  }
  
  if (length(cols_list) == 0) return(matrix(0, nrow = nrow(df), ncol = 0))
  
  mat <- do.call(cbind, cols_list)
  return(mat)
}

#' Scale an interordinal column (intervals)
#' @param df Data frame
#' @param col Column name
#' @param thresholds Numeric vector of thresholds
#' @return A matrix of binary columns
scale_interordinal <- function(df, col, thresholds = NULL) {
  # Interordinal scaling combines both <= and >= ordinal columns
  return(scale_ordinal(df, col, thresholds, direction = "both"))
}

#' Check if a vector is already binary (0/1 or TRUE/FALSE)
#' @param vec Vector to check
#' @return Boolean
is_binary_vector <- function(vec) {
  clean_vec <- vec[!is.na(vec) & !is.nan(vec)]
  if (length(clean_vec) == 0) return(TRUE)
  
  unique_vals <- unique(clean_vec)
  
  # Check if values are sub-set of c(0, 1) or c(FALSE, TRUE)
  if (is.logical(unique_vals)) return(TRUE)
  if (is.numeric(unique_vals) && all(unique_vals %in% c(0, 1))) return(TRUE)
  
  return(FALSE)
}

#' Scale a whole dataframe based on a configuration list
#' @param df Data frame
#' @param config A named list where keys are column names, and values are lists with:
#'        - type: "nominal", "ordinal", "interordinal", "binary" (or "asis"), or "ignore"
#'        - thresholds: numeric thresholds (for ordinal/interordinal)
#'        - direction: "le", "ge" (for ordinal)
#' @return A binary data frame with row names preserved from df
perform_conceptual_scaling <- function(df, config = list()) {
  mats <- list()
  col_names <- colnames(df)
  
  for (col in col_names) {
    # If the column has a config, use it. Otherwise, default to intelligent scaling
    col_config <- config[[col]]
    
    if (is.null(col_config)) {
      # Smart default:
      # If binary, keep as is
      # If numeric, default to ordinal ("le") using unique values
      # If character/factor, default to nominal
      if (is_binary_vector(df[[col]])) {
        type <- "binary"
      } else if (is.numeric(df[[col]])) {
        type <- "ordinal"
        direction <- "le"
        thresholds <- NULL
      } else {
        type <- "nominal"
      }
    } else {
      type <- col_config$type
      thresholds <- col_config$thresholds
      direction <- if (!is.null(col_config$direction)) col_config$direction else "le"
    }
    
    if (type == "ignore") {
      next
    }
    
    if (type == "binary") {
      # Convert logical to 0/1, force numeric format
      binary_col <- as.numeric(df[[col]])
      binary_col[is.na(binary_col)] <- 0
      mat <- matrix(binary_col, ncol = 1)
      colnames(mat) <- col
      mats[[col]] <- mat
    } else if (type == "nominal") {
      mats[[col]] <- scale_nominal(df, col)
    } else if (type == "ordinal") {
      mats[[col]] <- scale_ordinal(df, col, thresholds, direction)
    } else if (type == "interordinal") {
      mats[[col]] <- scale_interordinal(df, col, thresholds)
    }
  }
  
  if (length(mats) == 0) {
    # If nothing was scaled, return a matrix of zeros
    return(data.frame(row.names = rownames(df)))
  }
  
  combined_mat <- do.call(cbind, mats)
  res_df <- as.data.frame(combined_mat)
  rownames(res_df) <- rownames(df)
  
  return(res_df)
}
