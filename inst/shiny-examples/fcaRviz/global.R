# ==============================================================================
# global.R - VERSIÓN 3.2 (CON VISUALIZACIÓN JERÁRQUICA OPTIMIZADA)
# ==============================================================================

# 1. CARGA DE LIBRERÍAS
library(shiny)
library(bslib)
library(DT)
library(igraph)
library(visNetwork)
library(fcaR)
library(Matrix)
library(httr)
library(jsonlite)
library(dplyr)
library(glue)
library(shinycssloaders)
library(shinyWidgets)
library(shinyalert)
library(datamods)
library(xml2)
library(readxl)
library(rmarkdown)
library(shinyjs)
library(rhandsontable)

# Librerías de Reglas
library(arules)
library(arulesViz)
library(plotly)

source("uiUploadData.R")
source("uiBasicOperations.R")
source("uiImplications.R")
source("uiConcepts.R")
source("scaling_helpers.R")

# Paralelismo
library(future)
library(promises)
plan(multisession)

# TEMA
my_theme <- bs_theme(
  version = 5,
  bootswatch = "zephyr",
  base_font = font_google("Inter"),
  heading_font = font_google("Playfair Display"),
  primary = "#2c3e50",
  "enable-rounded" = TRUE
)

# --- CONEXIÓN API ---
returnNames <- function(){
  url <- "https://api.github.com/repos/fcatools/contexts/contents/contexts"
  response <- GET(url)
  if (status_code(response) == 200) {
    content_text <- content(response, as = "text", encoding = "UTF-8")
    content_json <- fromJSON(content_text)
    files <- content_json$name[grepl("\\.cxt$", content_json$name)]
    return(files)
  } else {
    return(NULL)
  }
}

selectOptions <- function(){
  files <- returnNames()
  if(is.null(files)) return(list("Error. Connection failed.", "Error"))
  meta_url <- "https://fcarepository.org/contexts.yaml"
  meta_data <- tryCatch(yaml::read_yaml(meta_url), error = function(e) NULL)
  titles <- unlist(lapply(files, function(x) {
    if(!is.null(meta_data) && !is.null(meta_data[[x]]$title)) return(meta_data[[x]]$title) else return(x)
  }))
  return(list(files, titles))
}

returnFCFromRepo <- function(option){
  URL <- glue::glue("https://github.com/fcatools/contexts/raw/main/contexts/{option}")
  file <- tempfile(fileext = ".cxt")
  try(download.file(URL, destfile = file, quiet = TRUE))
  fc <- FormalContext$new(file)
  return(fc)
}

# --- HELPERS ---

# Robust wrapper: LinCbO needs native symbols that may be missing in some installs/sessions.
safe_find_implications <- function(fc, verbose = FALSE, save_concepts = TRUE) {
  tryCatch(
    fc$find_implications(save_concepts = save_concepts, verbose = verbose, method = "LinCbO"),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("lincbo|LinCbO|_fcaR_binary", msg, ignore.case = TRUE)) {
        warning("LinCbO unavailable; using NextClosure instead.", call. = FALSE)
        fc$find_implications(save_concepts = save_concepts, verbose = verbose, method = "NextClosure")
      } else {
        stop(e)
      }
    }
  )
  invisible(fc)
}

# Arrow relation codes (fcaR 1.7+): 1 = ↙, 2 = ↗, 3 = ↕
arrow_code_to_symbol <- function(code) {
  switch(as.character(code),
         "1" = "\u2199",
         "2" = "\u2197",
         "3" = "\u2195",
         "")
}

is_binary_formal_context <- function(fc) {
  tryCatch(isTRUE(all(fc$I@x == 1)), error = function(e) FALSE)
}

has_arrow_relations_api <- function(fc) {
  is.object(fc) && inherits(fc, "FormalContext") &&
    is.function(fc$calculate_arrow_relations) && is.function(fc$get_arrow_relations)
}

get_context_df <- function(fc, show_arrows = FALSE) {
  if (is.null(fc)) return(data.frame())
  mat <- t(as.matrix(fc$I))
  if (is.null(rownames(mat))) rownames(mat) <- paste0("Obj", 1:nrow(mat))
  if (is.null(colnames(mat))) colnames(mat) <- paste0("Att", 1:ncol(mat))

  display <- matrix("", nrow = nrow(mat), ncol = ncol(mat),
                    dimnames = list(rownames(mat), colnames(mat)))
  display[mat > 0] <- "X"

  if (isTRUE(show_arrows)) {
    if (!has_arrow_relations_api(fc)) {
      warning("Arrow relations require fcaR >= 1.7.0.", call. = FALSE)
    } else if (!is_binary_formal_context(fc)) {
      warning("Arrow relations are only defined for binary contexts.", call. = FALSE)
    } else {
      tryCatch({
        fc$calculate_arrow_relations()
        arrows <- fc$get_arrow_relations()
        if (!identical(dim(arrows), dim(mat))) {
          arrows <- arrows[rownames(mat), colnames(mat), drop = FALSE]
        }
        empty_cells <- which(mat == 0, arr.ind = TRUE)
        if (nrow(empty_cells) > 0) {
          arrow_codes <- arrows[empty_cells]
          display[empty_cells] <- vapply(arrow_codes, arrow_code_to_symbol, character(1))
        }
      }, error = function(e) {
        warning("Could not compute arrow relations: ", conditionMessage(e), call. = FALSE)
      })
    }
  }

  df <- as.data.frame(display, stringsAsFactors = FALSE)
  df <- cbind(Objects = rownames(mat), df)
  return(df)
}

clean_latex_names <- function(names_vec) {
  if (length(names_vec) == 0) return(character(0))
  names_vec <- gsub("\\\\mathrm\\{([^}]*)\\}", "\\1", names_vec)
  names_vec <- gsub("\\\\ensuremath\\{([^}]*)\\}", "\\1", names_vec)
  names_vec <- gsub("\\\\left|\\\\right|\\\\\\(|\\\\\\)|\\$|\\\\,", "", names_vec)
  names_vec <- gsub("\\\\_", "_", names_vec)
  return(names_vec)
}

get_concepts_dataframe <- function(fc_concepts, fc = NULL) {
  n <- fc_concepts$size()
  if (n == 0) {
    return(data.frame(
      id = integer(0),
      objects = character(0),
      attributes = character(0),
      stringsAsFactors = FALSE
    ))
  }

  extents_mat <- as.matrix(fc_concepts$extents())
  intents_mat <- as.matrix(fc_concepts$intents())

  if (!is.null(fc)) {
    obj_names <- fc$objects
    att_names <- fc$attributes
  } else {
    obj_names <- rownames(extents_mat)
    att_names <- rownames(intents_mat)
  }

  if(is.null(obj_names)) obj_names <- paste0("Obj", 1:nrow(extents_mat))
  if(is.null(att_names)) att_names <- paste0("Att", 1:nrow(intents_mat))

  obj_names <- clean_latex_names(obj_names)
  att_names <- clean_latex_names(att_names)

  col_to_string <- function(logic_col, names_vec) {
    indices <- which(logic_col > 0)
    if (length(indices) == 0) return("{}")
    paste(names_vec[indices], collapse = ", ")
  }

  objects_str <- unname(apply(extents_mat, 2, col_to_string, names_vec = obj_names))
  attributes_str <- unname(apply(intents_mat, 2, col_to_string, names_vec = att_names))

  data.frame(id = seq_len(n), objects = objects_str, attributes = attributes_str, stringsAsFactors = FALSE)
}

get_implications_dataframe <- function(imp_set) {
  n_rules <- imp_set$cardinality()
  if (n_rules == 0) return(data.frame(Rule=character(), `if`=character(), then=character(), Support=numeric(), check.names = FALSE))

  lhs_mat <- as.matrix(imp_set$get_LHS_matrix())
  rhs_mat <- as.matrix(imp_set$get_RHS_matrix())

  # In fcaR, the LHS matrix has attributes as rows and rules as columns.
  # If it is a square matrix (rules == attributes), we must prioritize margin = 2.
  if (ncol(lhs_mat) == n_rules) {
    margin <- 2
    att_names <- rownames(lhs_mat)
    if(is.null(att_names)) att_names <- paste0("Att", 1:nrow(lhs_mat))
  } else if (nrow(lhs_mat) == n_rules) {
    margin <- 1
    att_names <- colnames(lhs_mat)
    if(is.null(att_names)) att_names <- paste0("Att", 1:ncol(lhs_mat))
  } else {
    margin <- 2
    att_names <- rownames(lhs_mat)
    if(is.null(att_names)) att_names <- paste0("Att", 1:nrow(lhs_mat))
  }
  att_names <- clean_latex_names(att_names)

  col_to_string <- function(vec, names) {
    indices <- which(vec > 0)
    if(length(indices) == 0) return("{}")
    paste(names[indices], collapse = ", ")
  }

  lhs_str <- unname(apply(lhs_mat, margin, col_to_string, names = att_names))
  rhs_str <- unname(apply(rhs_mat, margin, col_to_string, names = att_names))

  supp_vals <- tryCatch(imp_set$support(), error = function(e) numeric(0))
  if (length(supp_vals) != n_rules) {
    supp_vals <- rep(NA, n_rules)
  } else {
    supp_vals <- round(supp_vals, 4)
  }

  data.frame(Rule = 1:n_rules, `if` = lhs_str, then = rhs_str, Support = supp_vals, stringsAsFactors = FALSE, check.names = FALSE)
}

# --- VISUALIZACIÓN GRAFO: REDUCED LABELING ---
showPlot <- function(fc){
  g_vis <- getGraph(fc$concepts, fc)
  showPlot2(g_vis)
}

map_to_gradient <- function(values, low_color, high_color) {
  n <- length(values)
  if (n == 0) return(character(0))
  
  # Clean NAs
  values[is.na(values)] <- 0
  
  min_val <- min(values, na.rm = TRUE)
  max_val <- max(values, na.rm = TRUE)
  
  if (max_val == min_val) {
    return(rep(high_color, n))
  }
  
  normalized <- (values - min_val) / (max_val - min_val)
  ramp <- grDevices::colorRampPalette(c(low_color, high_color))(101)
  color_indices <- round(normalized * 100) + 1
  color_indices <- pmax(1, pmin(101, color_indices))
  return(ramp[color_indices])
}

get_concept_irreducible_status <- function(real_id, concepts, fc = NULL) {
  exts <- as.matrix(concepts$extents())
  ints <- as.matrix(concepts$intents())
  
  parents_sub <- concepts$upper_neighbours(concepts[real_id])
  children_sub <- concepts$lower_neighbours(concepts[real_id])
  
  if (parents_sub$size() > 0) {
    parent_ints <- as.matrix(parents_sub$intents())
    union_parents <- base::rowSums(parent_ints) > 0
    new_atts <- which(ints[, real_id] == 1 & !union_parents)
  } else {
    new_atts <- which(ints[, real_id] == 1)
  }
  
  if (children_sub$size() > 0) {
    child_exts <- as.matrix(children_sub$extents())
    union_children <- base::rowSums(child_exts) > 0
    new_objs <- which(exts[, real_id] == 1 & !union_children)
  } else {
    new_objs <- which(exts[, real_id] == 1)
  }
  
  has_atts <- length(new_atts) > 0
  has_objs <- length(new_objs) > 0
  
  if (!is.null(fc)) {
    obj_names <- fc$objects
    att_names <- fc$attributes
  } else {
    obj_names <- rownames(exts)
    att_names <- rownames(ints)
  }
  if(is.null(obj_names)) obj_names <- paste0("Obj", 1:nrow(exts))
  if(is.null(att_names)) att_names <- paste0("Att", 1:nrow(ints))
  obj_names <- clean_latex_names(obj_names)
  att_names <- clean_latex_names(att_names)
  
  introduced_atts <- att_names[new_atts]
  introduced_objs <- obj_names[new_objs]
  
  type_str <- "Intersection"
  color <- "#E0E0E0"
  if (has_atts && !has_objs) {
    type_str <- "Meet-Irreducible"
    color <- "#FFD580"
  } else if (!has_atts && has_objs) {
    type_str <- "Join-Irreducible"
    color <- "#97C2FC"
  } else if (has_atts && has_objs) {
    type_str <- "Double-Irreducible (Meet + Join)"
    color <- "#90EE90"
  }
  
  return(list(
    type = type_str,
    color = color,
    introduced_attributes = introduced_atts,
    introduced_objects = introduced_objs
  ))
}

get_concept_tooltip <- function(real_id, labels_vec_val, objects_str, attributes_str, color_mode, metrics, concepts = NULL) {
  type_str <- NULL
  if (!is.null(concepts)) {
    status <- tryCatch(get_concept_irreducible_status(real_id, concepts), error = function(e) NULL)
    if (!is.null(status)) {
      type_str <- status$type
    }
  }

  tooltip_html <- paste0(
    "<div style='text-align:left; font-family: Inter, sans-serif;'>",
    "<b>Concept ID:</b> C", real_id, "<br>"
  )
  if (!is.null(type_str)) {
    tooltip_html <- paste0(tooltip_html, "<b>Type:</b> ", type_str, "<br>")
  }
  tooltip_html <- paste0(
    tooltip_html,
    "<b>Reduced Label:</b> ", ifelse(labels_vec_val=="", "-", gsub("\n", ", ", labels_vec_val)), "<br><hr>",
    "<b>Full Extent:</b> {", objects_str, "}<br>",
    "<b>Full Intent:</b> {", attributes_str, "}"
  )
  
  if (!is.null(metrics)) {
    tooltip_html <- paste0(tooltip_html, "<hr>")
    
    # Stability
    stab_val <- if(length(metrics$stability) >= real_id) round(metrics$stability[real_id], 4) else NA
    if (!is.na(stab_val)) {
      if (color_mode == "stability") {
        tooltip_html <- paste0(tooltip_html, "<span style='display:inline-block; font-weight:bold; color:#4A148C; background-color:#F3E5F5; padding:2px 6px; border-radius:4px; margin-bottom:2px;'>Stability: ", stab_val, "</span><br>")
      } else {
        tooltip_html <- paste0(tooltip_html, "<b>Stability:</b> ", stab_val, "<br>")
      }
    }
    
    # Separation
    sep_val <- if(length(metrics$separation) >= real_id) round(metrics$separation[real_id], 4) else NA
    if (!is.na(sep_val)) {
      if (color_mode == "separation") {
        tooltip_html <- paste0(tooltip_html, "<span style='display:inline-block; font-weight:bold; color:#004D40; background-color:#E0F2F1; padding:2px 6px; border-radius:4px; margin-bottom:2px;'>Separation: ", sep_val, "</span><br>")
      } else {
        tooltip_html <- paste0(tooltip_html, "<b>Separation:</b> ", sep_val, "<br>")
      }
    }
    
    # Fuzzy Density
    dens_val <- if(length(metrics$density) >= real_id) round(metrics$density[real_id], 4) else NA
    if (!is.na(dens_val)) {
      if (color_mode == "density") {
        tooltip_html <- paste0(tooltip_html, "<span style='display:inline-block; font-weight:bold; color:#E65100; background-color:#FFF3E0; padding:2px 6px; border-radius:4px;'>Fuzzy Density: ", dens_val, "</span>")
      } else {
        tooltip_html <- paste0(tooltip_html, "<b>Fuzzy Density:</b> ", dens_val)
      }
    }
  }
  
  tooltip_html <- paste0(tooltip_html, "</div>")
  return(tooltip_html)
}

# Association rule details for a cover edge (parent -> child) in the concept lattice.
get_association_rule_details <- function(from_id, to_id, fc) {
  if (is.null(fc)) return(NULL)
  from_id <- as.numeric(from_id)
  to_id <- as.numeric(to_id)
  n <- fc$concepts$size()
  if (is.na(from_id) || is.na(to_id) || from_id < 1 || to_id < 1 || from_id > n || to_id > n) {
    return(NULL)
  }

  exts <- as.matrix(fc$concepts$extents())
  ints <- as.matrix(fc$concepts$intents())
  att_names <- if (!is.null(fc$attributes)) clean_latex_names(fc$attributes) else {
    clean_latex_names(rownames(ints))
  }
  if (is.null(att_names)) att_names <- paste0("Att", seq_len(nrow(ints)))

  size_from <- sum(exts[, from_id])
  size_to <- sum(exts[, to_id])
  N <- length(fc$objects)
  if (is.na(N) || N == 0) N <- nrow(exts)
  if (is.na(N) || N == 0) N <- 1
  conf <- if (size_from > 0) size_to / size_from else 0

  # A cover edge goes from a more general concept to a more specific one:
  # intent(parent) is contained in intent(child).  The induced rule is the
  # full parent intent -> the attributes introduced by the child.
  lhs_atts <- att_names[ints[, from_id] == 1]
  rhs_atts <- att_names[ints[, to_id] == 1 & ints[, from_id] == 0]

  list(
    from_id = from_id,
    to_id = to_id,
    lhs = lhs_atts,
    rhs = rhs_atts,
    confidence = conf,
    parent_support_pct = (size_from / N) * 100,
    parent_support_n = size_from,
    child_support_pct = (size_to / N) * 100,
    child_support_n = size_to,
    n_objects = N
  )
}

build_lattice_association_rule_html <- function(from_id, to_id, fc) {
  d <- get_association_rule_details(from_id, to_id, fc)
  if (is.null(d)) return("Association rule between parent and child concepts.")

  lhs_str <- if (length(d$lhs) > 0) paste(d$lhs, collapse = ", ") else "\u2205"
  rhs_str <- if (length(d$rhs) > 0) paste(d$rhs, collapse = ", ") else "\u2205"

  sprintf(
    paste0(
      "<b>Association Rule:</b> {%s} \u2192 {%s}<br>",
      "<b>Confidence:</b> %.1f%%<br>",
      "<b>Parent support:</b> %.1f%% (%d objs)<br>",
      "<b>Child support:</b> %.1f%% (%d objs)"
    ),
    lhs_str, rhs_str,
    d$confidence * 100, d$parent_support_pct, d$parent_support_n,
    d$child_support_pct, d$child_support_n
  )
}

# Compact info panel for the lattice dashboard (top-right overlay).
build_lattice_info_panel <- function(title, body_html, kind = "concept") {
  badge <- switch(
    kind,
    concept = span(class = "badge bg-primary", "Concept"),
    edge = span(class = "badge bg-success", "Association rule"),
    span(class = "badge bg-secondary", "Info")
  )
  div(
    class = "lattice-info-panel-inner",
    div(
      class = "lattice-info-panel-header",
      div(class = "d-flex align-items-center gap-2", badge, strong(title)),
      actionButton(
        "btnCloseLatticeInfo",
        label = NULL,
        icon = icon("times"),
        class = "btn btn-sm btn-light lattice-info-close",
        title = "Close"
      )
    ),
    div(class = "lattice-info-panel-body", HTML(body_html))
  )
}

format_node_label <- function(atts_str, objs_str, real_id, labeling_mode, dt_objects_idx = "", dt_attributes_idx = "") {
  if (labeling_mode == "id_only") {
    return(paste0("C", real_id))
  }
  
  if (labeling_mode == "full") {
    return(paste0("Ext: {", dt_objects_idx, "}\nInt: {", dt_attributes_idx, "}"))
  }
  
  has_atts <- nchar(atts_str) > 0
  has_objs <- nchar(objs_str) > 0
  
  if (labeling_mode == "classic_reduced") {
    if (has_atts && has_objs) {
      return(paste0(atts_str, "\n──────────\n", objs_str))
    } else if (has_atts) {
      return(atts_str)
    } else if (has_objs) {
      return(objs_str)
    } else {
      return("")
    }
  }
  
  if (labeling_mode == "attributes_only") {
    return(ifelse(has_atts, atts_str, ""))
  }
  
  if (labeling_mode == "objects_only") {
    return(ifelse(has_objs, objs_str, ""))
  }
  
  # Fallback for "reduced"
  if (has_atts) return(atts_str)
  if (has_objs) return(objs_str)
  return("")
}

getGraph <- function(concepts, fc = NULL, filter_ids = NULL, scale_support = FALSE, labeling_mode = "reduced", color_mode = "irreducibles", metrics = NULL){
  n_total <- concepts$size()
  if (is.null(filter_ids)) {
    target_ids <- 1:n_total
  } else {
    target_ids <- filter_ids
  }

  n <- length(target_ids)

  if (n == 0) {
    nodes <- data.frame(
      id = "empty",
      label = "No matches",
      title = "No concepts match the selected filters",
      color.background = "#E0E0E0",
      color.border = "#666666",
      shape = "box",
      font.size = 14,
      stringsAsFactors = FALSE
    )
    return(list(nodes = nodes, edges = data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)))
  }

  sub_concepts <- concepts[target_ids]
  dt <- get_concepts_dataframe(sub_concepts, fc)

  if(n == 1) {
    real_id <- target_ids[1]
    exts <- as.matrix(sub_concepts$extents())
    ints <- as.matrix(sub_concepts$intents())

    if (!is.null(fc)) {
      obj_names <- fc$objects
      att_names <- fc$attributes
    } else {
      obj_names <- rownames(exts)
      att_names <- rownames(ints)
    }
    if(is.null(obj_names)) obj_names <- paste0("Obj", 1:nrow(exts))
    if(is.null(att_names)) att_names <- paste0("Att", 1:nrow(ints))
    obj_names <- clean_latex_names(obj_names)
    att_names <- clean_latex_names(att_names)

    new_atts_idx <- which(ints[, 1] == 1)
    new_objs_idx <- which(exts[, 1] == 1)

    atts_str <- paste(att_names[new_atts_idx], collapse = "\n")
    objs_str <- paste(obj_names[new_objs_idx], collapse = "\n")

    has_atts <- length(new_atts_idx) > 0
    has_objs <- length(new_objs_idx) > 0

    # Calculate metrics on the fly if needed
    if (is.null(metrics) && !is.null(fc) && color_mode %in% c("stability", "separation", "density")) {
      metrics <- list()
      if (color_mode == "stability") {
        metrics$stability <- tryCatch(concepts$stability(), error = function(e) rep(NA, concepts$size()))
      } else if (color_mode == "separation") {
        metrics$separation <- tryCatch(concepts$separation(), error = function(e) rep(NA, concepts$size()))
      } else if (color_mode == "density") {
        metrics$density <- tryCatch(concepts$density(fc$I), error = function(e) rep(NA, concepts$size()))
      }
    }

    bg_color <- "#E0E0E0"
    if (color_mode == "stability" && !is.null(metrics$stability)) {
      bg_color <- map_to_gradient(metrics$stability[real_id], "#F3E5F5", "#4A148C")
    } else if (color_mode == "separation" && !is.null(metrics$separation)) {
      bg_color <- map_to_gradient(metrics$separation[real_id], "#E0F2F1", "#004D40")
    } else if (color_mode == "density" && !is.null(metrics$density)) {
      bg_color <- map_to_gradient(metrics$density[real_id], "#FFF3E0", "#E65100")
    } else {
      if (has_atts && !has_objs) {
        bg_color <- "#FFD580"
      } else if (!has_atts && has_objs) {
        bg_color <- "#97C2FC"
      } else if (has_atts && has_objs) {
        bg_color <- "#90EE90"
      }
    }

    node_label <- format_node_label(atts_str, objs_str, real_id, labeling_mode, dt$objects[1], dt$attributes[1])

    nodes <- data.frame(
      id = as.character(real_id),
      label = node_label,
      color.background = bg_color,
      color.border = "#666666",
      shape = "dot",
      font.size = 16,
      title = get_concept_tooltip(real_id, ifelse(nchar(atts_str) > 0, atts_str, ifelse(nchar(objs_str) > 0, objs_str, "")), dt$objects[1], dt$attributes[1], color_mode, metrics, concepts = concepts),
      stringsAsFactors = FALSE
    )
    if (isTRUE(scale_support)) {
      nodes$size <- 25
    }
    return(list(nodes = nodes, edges = data.frame(from=character(0), to=character(0), stringsAsFactors = FALSE)))
  }

  # Calculate metrics on the fly if needed
  if (is.null(metrics) && !is.null(fc) && color_mode %in% c("stability", "separation", "density")) {
    metrics <- list()
    if (color_mode == "stability") {
      metrics$stability <- tryCatch(concepts$stability(), error = function(e) rep(NA, concepts$size()))
    } else if (color_mode == "separation") {
      metrics$separation <- tryCatch(concepts$separation(), error = function(e) rep(NA, concepts$size()))
    } else if (color_mode == "density") {
      metrics$density <- tryCatch(concepts$density(fc$I), error = function(e) rep(NA, concepts$size()))
    }
  }

  # Matrices base
  exts <- as.matrix(sub_concepts$extents())
  ints <- as.matrix(sub_concepts$intents())
  sizes <- base::colSums(exts)

  # Matriz de Adyacencia
  M <- matrix(0, nrow = n, ncol = n)
  for(padre in 1:n) {
    for(hijo in 1:n) {
      if(padre != hijo && sizes[padre] > sizes[hijo]) {
        intersection_size <- sum(exts[, hijo] & exts[, padre])
        if(intersection_size == sizes[hijo]) { M[padre, hijo] <- 1 }
      }
    }
  }

  # Reducción Transitiva
  path_len_2 <- (M %*% M) > 0
  M_reduced <- M & (!path_len_2)

  # --- ETIQUETAS REDUCIDAS ---
  if (!is.null(fc)) {
    obj_names <- fc$objects
    att_names <- fc$attributes
  } else {
    obj_names <- rownames(exts)
    att_names <- rownames(ints)
  }
  if(is.null(obj_names)) obj_names <- paste0("Obj", 1:nrow(exts))
  if(is.null(att_names)) att_names <- paste0("Att", 1:nrow(ints))
  obj_names <- clean_latex_names(obj_names)
  att_names <- clean_latex_names(att_names)

  labels_vec_atts <- character(n)
  labels_vec_objs <- character(n)
  colors_vec <- character(n)

  for(i in 1:n) {
    # Reduced Intent (Atributos Nuevos)
    parents <- which(M_reduced[, i] == 1)
    if(length(parents) > 0) {
      parent_ints_cols <- ints[, parents, drop=FALSE]
      union_parents <- base::rowSums(parent_ints_cols) > 0
      new_atts_idx <- which(ints[, i] == 1 & !union_parents)
    } else {
      new_atts_idx <- which(ints[, i] == 1)
    }

    # Reduced Extent (Objetos Nuevos)
    children <- which(M_reduced[i, ] == 1)
    if(length(children) > 0) {
      child_exts_cols <- exts[, children, drop=FALSE]
      union_children <- base::rowSums(child_exts_cols) > 0
      new_objs_idx <- which(exts[, i] == 1 & !union_children)
    } else {
      new_objs_idx <- which(exts[, i] == 1)
    }

    # Etiqueta
    labels_vec_atts[i] <- paste(att_names[new_atts_idx], collapse = "\n")
    labels_vec_objs[i] <- paste(obj_names[new_objs_idx], collapse = "\n")

    # Color
    has_atts <- length(new_atts_idx) > 0
    has_objs <- length(new_objs_idx) > 0

    if (has_atts && !has_objs) {
      colors_vec[i] <- "#FFD580" # Naranja
    } else if (!has_atts && has_objs) {
      colors_vec[i] <- "#97C2FC" # Azul
    } else if (has_atts && has_objs) {
      colors_vec[i] <- "#90EE90" # Verde
    } else {
      colors_vec[i] <- "#E0E0E0" # Gris
    }
  }

  if (color_mode == "stability" && !is.null(metrics$stability)) {
    colors_vec <- map_to_gradient(metrics$stability[target_ids], "#F3E5F5", "#4A148C")
  } else if (color_mode == "separation" && !is.null(metrics$separation)) {
    colors_vec <- map_to_gradient(metrics$separation[target_ids], "#E0F2F1", "#004D40")
  } else if (color_mode == "density" && !is.null(metrics$density)) {
    colors_vec <- map_to_gradient(metrics$density[target_ids], "#FFF3E0", "#E65100")
  }

  g <- igraph::graph_from_adjacency_matrix(M_reduced, mode = "directed")
  vis_data <- visNetwork::toVisNetworkData(g)

  nodes <- vis_data$nodes
  nodes$id <- as.character(nodes$id)
  idx <- match(nodes$id, as.character(dt$id))

  nodes$label <- sapply(1:nrow(nodes), function(i) {
    local_id <- as.numeric(nodes$id[i])
    real_id <- target_ids[local_id]
    format_node_label(
      labels_vec_atts[local_id], 
      labels_vec_objs[local_id], 
      real_id, 
      labeling_mode, 
      dt$objects[idx[i]], 
      dt$attributes[idx[i]]
    )
  })

  nodes$color.background <- colors_vec
  nodes$color.border <- "#666666"
  nodes$shape <- "dot"
  nodes$font.size <- 16

  nodes$title <- sapply(1:nrow(nodes), function(i) {
    local_id <- as.numeric(nodes$id[i])
    real_id <- target_ids[local_id]
    tooltip_lbl <- if (nchar(labels_vec_atts[local_id]) > 0) labels_vec_atts[local_id] else (if (nchar(labels_vec_objs[local_id]) > 0) labels_vec_objs[local_id] else "")
    get_concept_tooltip(real_id, tooltip_lbl, dt$objects[idx[i]], dt$attributes[idx[i]], color_mode, metrics, concepts = concepts)
  })

  # Sizing
  if (isTRUE(scale_support)) {
    local_idx <- as.numeric(nodes$id)
    node_sizes <- sizes[local_idx]
    max_size <- max(sizes, na.rm = TRUE)
    if (is.na(max_size) || max_size == 0) max_size <- 1
    nodes$size <- 14 + 18 * (node_sizes / max_size)
  }

  # Map local node IDs to original Concept IDs
  nodes$id <- as.character(target_ids[as.numeric(nodes$id)])

  vis_data$nodes <- nodes
  if(!is.null(vis_data$edges) && nrow(vis_data$edges) > 0){
    all_exts <- as.matrix(concepts$extents())
    all_sizes <- base::colSums(all_exts)
    N <- if (!is.null(fc)) length(fc$objects) else nrow(all_exts)
    if (is.na(N) || N == 0) N <- 1
    
    from_orig_ids <- target_ids[as.numeric(vis_data$edges$from)]
    to_orig_ids <- target_ids[as.numeric(vis_data$edges$to)]
    
    edge_confidences <- sapply(1:nrow(vis_data$edges), function(i) {
      size_from <- all_sizes[from_orig_ids[i]]
      size_to <- all_sizes[to_orig_ids[i]]
      if (size_from > 0) size_to / size_from else 0
    })
    
    vis_data$edges$width <- 1 + 5 * edge_confidences
    vis_data$edges$title <- sapply(1:nrow(vis_data$edges), function(i) {
      size_from <- all_sizes[from_orig_ids[i]]
      size_to <- all_sizes[to_orig_ids[i]]
      conf <- edge_confidences[i]
      sprintf("Association Rule (LHS -> RHS)<br>Confidence: %.1f%%<br>Parent Support: %.1f%% (%d objs)<br>Child Support: %.1f%% (%d objs)",
              conf * 100, (size_from / N) * 100, size_from, (size_to / N) * 100, size_to)
    })
    vis_data$edges$from <- as.character(from_orig_ids)
    vis_data$edges$to <- as.character(to_orig_ids)
    vis_data$edges$id <- paste0("e:", vis_data$edges$from, ":", vis_data$edges$to)
  }
  return(vis_data)
}

# Remove native vis.js tooltips; info is shown in the Shiny overlay panel.
strip_lattice_vis_tooltips <- function(g) {
  if (is.null(g)) return(g)
  if (!is.null(g$nodes) && nrow(g$nodes) > 0 && "title" %in% names(g$nodes)) {
    g$nodes$title <- NULL
  }
  if (!is.null(g$edges) && nrow(g$edges) > 0) {
    if (!"id" %in% names(g$edges)) {
      g$edges$id <- paste0("e:", g$edges$from, ":", g$edges$to)
    }
    if ("title" %in% names(g$edges)) {
      g$edges$title <- NULL
    }
    g$edges$selectable <- FALSE
  }
  g
}

# --- PLOT ESTÁNDAR (Sugiyama) ---
showPlot2 <- function(vis_data){
  visNetwork::visNetwork(nodes = vis_data$nodes, edges = vis_data$edges) %>%
    visNetwork::visIgraphLayout(layout = "layout_with_sugiyama") %>%
    visNetwork::visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical", labelOnly = FALSE), nodesIdSelection = TRUE) %>%
    visNetwork::visEdges(arrows = list("to" = list(enabled = TRUE, scaleFactor = 0.5)), color = list(color = "#AAAAAA"), smooth = list(type = "cubicBezier", roundness = 0.5)) %>%
    visNetwork::visPhysics(stabilization = FALSE, barnesHut = list(gravitationalConstant = -2000)) %>%
    visNetwork::visInteraction(navigationButtons = TRUE, zoomView = TRUE)
}

# --- NUEVO: PLOT JERÁRQUICO OPTIMIZADO PARA RECOMENDACIÓN ---
# Esta función garantiza que el árbol de recomendación salga ordenado y separado.
showPlotHierarchy <- function(vis_data){
  visNetwork::visNetwork(nodes = vis_data$nodes, edges = vis_data$edges) %>%
    visNetwork::visHierarchicalLayout(
      direction = "UD",          # De Arriba a Abajo
      sortMethod = "directed",   # Orden estricto por jerarquía
      levelSeparation = 120,     # Separación vertical entre niveles
      nodeSpacing = 200,         # ¡CRITICO! Separación horizontal amplia para evitar solapamientos
      treeSpacing = 220,         # Separación entre árboles independientes
      blockShifting = TRUE,
      edgeMinimization = TRUE,
      parentCentralization = TRUE
    ) %>%
    visNetwork::visOptions(
      highlightNearest = list(enabled = TRUE, algorithm = "hierarchical"),
      nodesIdSelection = TRUE
    ) %>%
    visNetwork::visEdges(
      arrows = "to",
      color = list(color = "#AAAAAA"),
      smooth = list(type = "cubicBezier", forceDirection = "vertical", roundness = 0.4) # Curvas suaves verticales
    ) %>%
    visNetwork::visInteraction(navigationButtons = TRUE, zoomView = TRUE)
}

# --- SUBRETÍCULOS ---
find_one_concept <- function(lattice, closed_set) {
  intents <- lattice$intents()
  v <- as(as(as_vector(closed_set), "matrix"), "dgCMatrix")
  Matrix::which(fcaR:::.equal_sets(v, intents))
}
getOneConcept <- function(fc, attributes){
  S <- Set$new(fc$attributes)
  sapply(attributes, function(x){ do.call(S$assign, setNames(list(1), x)) })
  closed_set <- fc$closure(S)
  index <- find_one_concept(lattice = fc$concepts, closed_set = closed_set)
  return(index)
}
getSublattice2 <- function(concepts, i, t) {
  all_exts <- as.matrix(concepts$extents())
  ext_i <- all_exts[, i]; ext_t <- all_exts[, t]
  is_sup_t <- base::colSums(all_exts & ext_t) == sum(ext_t)
  is_sub_i <- base::colSums(all_exts & ext_i) == base::colSums(all_exts)
  indexes <- which(is_sup_t & is_sub_i)
  if(length(indexes) == 0) {
    is_sup_i <- base::colSums(all_exts & ext_i) == sum(ext_i)
    is_sub_t <- base::colSums(all_exts & ext_t) == base::colSums(all_exts)
    indexes <- which(is_sup_i & is_sub_t)
  }
  if(length(indexes) == 0) return(NULL)
  return(concepts$sublattice(indexes))
}
indexLowerNeighbours <- function(concepts, c){
  neighs <- concepts$lower_neighbours(c)
  if (neighs$size() == 0) return(numeric(0))
  all_intents <- as.matrix(concepts$intents())
  neigh_intents <- as.matrix(neighs$intents())
  indexes <- c()
  for(k in 1:ncol(neigh_intents)) {
    match <- which(base::colSums(all_intents == neigh_intents[, k]) == nrow(all_intents))
    if(length(match) > 0) indexes <- c(indexes, match[1])
  }
  return(indexes)
}

# --- HIGH-PERFORMANCE NEIGHBORHOOD EXPLORER (MICRO-MODE) ---
get_neighborhood_ids_binary <- function(center_id, intents_mat) {
  c_int <- intents_mat[, center_id]
  
  # Superconcepts: intent(concept) is a subset of intent(center)
  is_super <- colSums(intents_mat & !c_int) == 0
  super_idx <- which(is_super)
  super_idx <- setdiff(super_idx, center_id)
  
  direct_upper <- c()
  if (length(super_idx) > 0) {
    # Sorted by descending intent size (largest/closest to center first)
    intent_sizes <- colSums(intents_mat[, super_idx, drop=FALSE])
    sorted_super <- super_idx[order(-intent_sizes)]
    
    for (s in sorted_super) {
      s_int <- intents_mat[, s]
      others <- setdiff(direct_upper, s)
      if (length(others) == 0) {
        direct_upper <- c(direct_upper, s)
        next
      }
      # Check if s_int is a subset of any already-added direct_upper concept (others)
      # if s_int is subset of others, then s is higher than others, so not direct upper.
      is_redundant <- colSums(s_int & !intents_mat[, others, drop=FALSE]) == 0
      if (!any(is_redundant)) {
        direct_upper <- c(direct_upper, s)
      }
    }
  }
  
  # Subconcepts: intent(center) is a subset of intent(concept)
  is_sub <- colSums(c_int & !intents_mat) == 0
  sub_idx <- which(is_sub)
  sub_idx <- setdiff(sub_idx, center_id)
  
  direct_lower <- c()
  if (length(sub_idx) > 0) {
    # Sorted by ascending intent size (smallest/closest to center first)
    intent_sizes <- colSums(intents_mat[, sub_idx, drop=FALSE])
    sorted_sub <- sub_idx[order(intent_sizes)]
    
    for (s in sorted_sub) {
      s_int <- intents_mat[, s]
      others <- setdiff(direct_lower, s)
      if (length(others) == 0) {
        direct_lower <- c(direct_lower, s)
        next
      }
      # Check if any already-added direct_lower concept (others) is a subset of s_int
      # if others is subset of s_int, then s is lower than others, so not direct lower.
      is_redundant <- colSums(intents_mat[, others, drop=FALSE] & !s_int) == 0
      if (!any(is_redundant)) {
        direct_lower <- c(direct_lower, s)
      }
    }
  }
  
  return(list(center = center_id, upper = direct_upper, lower = direct_lower))
}

# Get local reduced labeling and color for a specific node in micro-mode
getLocalNodeLabelColor <- function(i, ints, exts, obj_names, att_names) {
  neigh_i <- get_neighborhood_ids_binary(i, ints)
  parents <- neigh_i$upper
  
  if (length(parents) > 0) {
    parent_ints_cols <- ints[, parents, drop=FALSE]
    union_parents <- base::rowSums(parent_ints_cols) > 0
    new_atts_idx <- which(ints[, i] == 1 & !union_parents)
  } else {
    new_atts_idx <- which(ints[, i] == 1)
  }
  
  children <- neigh_i$lower
  if (length(children) > 0) {
    child_exts_cols <- exts[, children, drop=FALSE]
    union_children <- base::rowSums(child_exts_cols) > 0
    new_objs_idx <- which(exts[, i] == 1 & !union_children)
  } else {
    new_objs_idx <- which(exts[, i] == 1)
  }
  
  atts_str <- paste(att_names[new_atts_idx], collapse = "\n")
  objs_str <- paste(obj_names[new_objs_idx], collapse = "\n")
  
  label_final <- ""
  if (nchar(atts_str) > 0) {
    label_final <- atts_str
  } else if (nchar(objs_str) > 0) {
    label_final <- objs_str
  } else {
    label_final <- ""
  }
  
  has_atts <- length(new_atts_idx) > 0
  has_objs <- length(new_objs_idx) > 0
  
  if (has_atts && !has_objs) {
    color <- "#FFD580" # Naranja
  } else if (!has_atts && has_objs) {
    color <- "#97C2FC" # Azul
  } else if (has_atts && has_objs) {
    color <- "#90EE90" # Verde
  } else {
    color <- "#E0E0E0" # Gris
  }
  
  return(list(label = label_final, color = color))
}

# Construct neighborhood graph for Micro Explorer
getMicroGraph <- function(concepts_all, center_id, neigh, fc = NULL, scale_support = FALSE, labeling_mode = "reduced", color_mode = "irreducibles", metrics = NULL) {
  node_ids <- unique(c(neigh$center, neigh$upper, neigh$lower))
  n_nodes <- length(node_ids)
  
  dt_all <- get_concepts_dataframe(concepts_all, fc)
  exts <- as.matrix(concepts_all$extents())
  ints <- as.matrix(concepts_all$intents())
  
  if (!is.null(fc)) {
    obj_names <- fc$objects
    att_names <- fc$attributes
  } else {
    obj_names <- rownames(exts)
    att_names <- rownames(ints)
  }
  obj_names <- clean_latex_names(obj_names)
  att_names <- clean_latex_names(att_names)
  
  labels_vec <- character(n_nodes)
  labels_vec_atts <- character(n_nodes)
  labels_vec_objs <- character(n_nodes)
  colors_vec <- character(n_nodes)
  
  # Calculate metrics on the fly if needed
  if (is.null(metrics) && !is.null(fc) && color_mode %in% c("stability", "separation", "density")) {
    metrics <- list()
    if (color_mode == "stability") {
      metrics$stability <- tryCatch(concepts_all$stability(), error = function(e) rep(NA, concepts_all$size()))
    } else if (color_mode == "separation") {
      metrics$separation <- tryCatch(concepts_all$separation(), error = function(e) rep(NA, concepts_all$size()))
    } else if (color_mode == "density") {
      metrics$density <- tryCatch(concepts_all$density(fc$I), error = function(e) rep(NA, concepts_all$size()))
    }
  }

  idx_match <- match(as.character(node_ids), as.character(dt_all$id))

  for (idx in 1:n_nodes) {
    nid <- node_ids[idx]
    
    # Calculate irreducible status
    status <- tryCatch(get_concept_irreducible_status(nid, concepts_all, fc), error = function(e) NULL)
    atts_str <- ""
    objs_str <- ""
    if (!is.null(status)) {
      atts_str <- paste(status$introduced_attributes, collapse = "\n")
      objs_str <- paste(status$introduced_objects, collapse = "\n")
    }
    
    labels_vec_atts[idx] <- atts_str
    labels_vec_objs[idx] <- objs_str
    
    res <- getLocalNodeLabelColor(nid, ints, exts, obj_names, att_names)
    labels_vec[idx] <- format_node_label(atts_str, objs_str, nid, labeling_mode, dt_all$objects[idx_match[idx]], dt_all$attributes[idx_match[idx]])
    
    if (color_mode == "stability" && !is.null(metrics$stability)) {
      colors_vec[idx] <- map_to_gradient(metrics$stability[nid], "#F3E5F5", "#4A148C")
    } else if (color_mode == "separation" && !is.null(metrics$separation)) {
      colors_vec[idx] <- map_to_gradient(metrics$separation[nid], "#E0F2F1", "#004D40")
    } else if (color_mode == "density" && !is.null(metrics$density)) {
      colors_vec[idx] <- map_to_gradient(metrics$density[nid], "#FFF3E0", "#E65100")
    } else {
      colors_vec[idx] <- res$color
    }
  }
  
  sizes_vec <- rep(25, n_nodes)
  if (isTRUE(scale_support)) {
    sizes <- base::colSums(exts)
    node_sizes <- sizes[node_ids]
    max_size <- max(sizes, na.rm = TRUE)
    if (is.na(max_size) || max_size == 0) max_size <- 1
    sizes_vec <- 14 + 18 * (node_sizes / max_size)
  }
  sizes_vec[node_ids == center_id] <- sizes_vec[node_ids == center_id] + 15
  
  nodes <- data.frame(
    id = as.character(node_ids),
    label = labels_vec,
    color.background = colors_vec,
    color.border = ifelse(node_ids == center_id, "#ff3333", "#666666"),
    borderWidth = ifelse(node_ids == center_id, 3, 1),
    shape = "dot",
    size = sizes_vec,
    font.size = ifelse(node_ids == center_id, 18, 14),
    font.bold = ifelse(node_ids == center_id, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  
  idx_match <- match(nodes$id, as.character(dt_all$id))
  
  nodes$title <- sapply(1:n_nodes, function(idx) {
    nid <- node_ids[idx]
    im <- idx_match[idx]
    tooltip_lbl <- if (nchar(labels_vec_atts[idx]) > 0) labels_vec_atts[idx] else (if (nchar(labels_vec_objs[idx]) > 0) labels_vec_objs[idx] else "")
    get_concept_tooltip(nid, tooltip_lbl, dt_all$objects[im], dt_all$attributes[im], color_mode, metrics, concepts = concepts_all)
  })
  
  edges <- data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
  if (length(neigh$upper) > 0) {
    edges <- rbind(edges, data.frame(
      from = as.character(neigh$upper),
      to = as.character(center_id),
      stringsAsFactors = FALSE
    ))
  }
  if (length(neigh$lower) > 0) {
    edges <- rbind(edges, data.frame(
      from = as.character(center_id),
      to = as.character(neigh$lower),
      stringsAsFactors = FALSE
    ))
  }

  if (nrow(edges) > 0) {
    all_exts <- as.matrix(concepts_all$extents())
    all_sizes <- base::colSums(all_exts)
    N <- if (!is.null(fc)) length(fc$objects) else nrow(all_exts)
    if (is.na(N) || N == 0) N <- 1
    edges$title <- vapply(seq_len(nrow(edges)), function(i) {
      from_id <- as.numeric(edges$from[i])
      to_id <- as.numeric(edges$to[i])
      size_from <- all_sizes[from_id]
      size_to <- all_sizes[to_id]
      conf <- if (size_from > 0) size_to / size_from else 0
      sprintf(
        "Association Rule (LHS -> RHS)<br>Confidence: %.1f%%<br>Parent Support: %.1f%% (%d objs)<br>Child Support: %.1f%% (%d objs)",
        conf * 100, (size_from / N) * 100, size_from, (size_to / N) * 100, size_to
      )
    }, character(1))
    edges$id <- paste0("e:", edges$from, ":", edges$to)
  }

  return(list(nodes = nodes, edges = edges))
}

# --- PARSER Y EXPORTADOR NATIVO DE FICHEROS .CEX (CONEXP XML) ---
parse_cex <- function(filepath) {
  doc <- read_xml(filepath)
  
  context_node <- xml_find_first(doc, "//Context")
  if (length(context_node) == 0 || is.na(context_node)) {
    stop("No <Context> element found in the .cex file.")
  }
  
  attr_nodes <- xml_find_all(context_node, "Attributes/Attribute")
  if (length(attr_nodes) == 0) {
    stop("No attributes found in the .cex file.")
  }
  
  attr_names <- xml_text(xml_find_all(attr_nodes, "Name"))
  attr_ids <- xml_attr(attr_nodes, "Identifier")
  if (any(is.na(attr_ids))) {
    attr_ids <- as.character(0:(length(attr_names) - 1))
  }
  
  attr_index_map <- setNames(1:length(attr_names), attr_ids)
  
  obj_nodes <- xml_find_all(context_node, "Objects/Object")
  if (length(obj_nodes) == 0) {
    stop("No objects found in the .cex file.")
  }
  
  obj_names <- xml_text(xml_find_all(obj_nodes, "Name"))
  
  mat <- matrix(0, nrow = length(obj_names), ncol = length(attr_names))
  colnames(mat) <- attr_names
  rownames(mat) <- obj_names
  
  for (i in seq_along(obj_nodes)) {
    obj_node <- obj_nodes[i]
    has_attr_nodes <- xml_find_all(obj_node, "Intent/HasAttribute")
    if (length(has_attr_nodes) > 0) {
      attr_refs <- xml_attr(has_attr_nodes, "AttributeIdentifier")
      indices <- attr_index_map[attr_refs]
      indices <- indices[!is.na(indices)]
      if (length(indices) > 0) {
        mat[i, indices] <- 1
      }
    }
  }
  
  return(mat)
}

export_cex <- function(fc, filepath) {
  mat <- t(as.matrix(fc$I))
  obj_names <- rownames(mat)
  attr_names <- colnames(mat)
  
  doc <- xml_new_root("ConceptualSystem")
  xml_add_child(doc, "Version", MajorNumber = "1", MinorNumber = "0")
  
  contexts_node <- xml_add_child(doc, "Contexts")
  context_node <- xml_add_child(contexts_node, "Context", Identifier = "0", Type = "Binary")
  
  attributes_node <- xml_add_child(context_node, "Attributes")
  for (j in seq_along(attr_names)) {
    attr_node <- xml_add_child(attributes_node, "Attribute", Identifier = as.character(j - 1))
    xml_add_child(attr_node, "Name", attr_names[j])
  }
  
  objects_node <- xml_add_child(context_node, "Objects")
  for (i in seq_along(obj_names)) {
    obj_node <- xml_add_child(objects_node, "Object", Identifier = as.character(i - 1))
    xml_add_child(obj_node, "Name", obj_names[i])
    
    intent_node <- xml_add_child(obj_node, "Intent")
    active_attrs <- which(mat[i, ] > 0)
    for (idx in active_attrs) {
      xml_add_child(intent_node, "HasAttribute", AttributeIdentifier = as.character(idx - 1))
    }
  }
  
  write_xml(doc, filepath)
}

# --- STATIC PLOT GENERATION FOR VECTOR EXPORTS (SVG/PDF) ---
plot_lattice_static <- function(concepts, fc = NULL, layout_val = "layout_with_sugiyama", filter_ids = NULL) {
  g_data <- getGraph(concepts, fc, filter_ids = filter_ids)
  nodes <- g_data$nodes
  edges <- g_data$edges
  
  if (nrow(nodes) == 0) {
    plot(NULL, xlim = c(0, 1), ylim = c(0, 1), type = "n", xlab = "", ylab = "", main = "Empty Lattice")
    return(NULL)
  }
  
  # Create an igraph object from the edges
  if (nrow(edges) > 0) {
    g_igraph <- igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
  } else {
    g_igraph <- igraph::make_empty_graph(n = nrow(nodes))
    igraph::V(g_igraph)$name <- nodes$id
  }
  
  # Apply layout matching the UI selection
  if (layout_val == "layout_with_sugiyama") {
    l <- igraph::layout_with_sugiyama(g_igraph)$layout
  } else {
    l <- igraph::layout_with_kk(g_igraph)
  }
  
  # Construct readable labels: Concept ID and reduced label if present
  lbls <- sapply(1:nrow(nodes), function(i) {
    cid <- nodes$id[i]
    r_lbl <- nodes$label[i]
    if (r_lbl != "") {
      # Keep newlines for multi-line layout in static plot
      paste0("C", cid, "\n", r_lbl)
    } else {
      paste0("C", cid)
    }
  })
  
  node_colors <- nodes$color.background
  
  # High-quality vector-friendly plot
  plot(
    g_igraph,
    layout = l,
    vertex.color = node_colors,
    vertex.frame.color = "#666666",
    vertex.shape = "circle",
    vertex.size = 18,
    vertex.label = lbls,
    vertex.label.color = "#333333",
    vertex.label.family = "sans",
    vertex.label.cex = 0.75,
    vertex.label.dist = 0,
    edge.arrow.size = 0.4,
    edge.color = "#999999",
    edge.width = 1.2,
    main = paste("Concept Lattice (", ifelse(layout_val == "layout_with_sugiyama", "Sugiyama", "Force Directed"), " Layout)", sep = "")
  )
}

# --- GENERATE GRAPHVIZ DOT REPRESENTATION OF HASSE DIAGRAM ---
generate_dot_code <- function(concepts, fc = NULL, filter_ids = NULL) {
  g_data <- getGraph(concepts, fc, filter_ids = filter_ids)
  nodes <- g_data$nodes
  edges <- g_data$edges
  
  dot <- c("digraph ConceptLattice {",
           "  // Graphviz DOT representation of FCA Concept Lattice",
           "  rankdir=TB;  // Top-to-bottom layout matching UI Sugiyama",
           "  node [shape=box, style=\"filled,rounded\", fontname=\"Helvetica\", fontsize=10, penwidth=1.5];",
           "  edge [color=\"#999999\", arrowhead=normal, arrowsize=0.6, penwidth=1.2];",
           "")
  
  for (i in 1:nrow(nodes)) {
    cid <- nodes$id[i]
    lbl <- nodes$label[i]
    bg_color <- nodes$color.background[i]
    
    if (lbl != "") {
      esc_lbl <- gsub("\n", "\\\\n", lbl)
      node_label <- paste0("C", cid, "\\n", esc_lbl)
    } else {
      node_label <- paste0("C", cid)
    }
    
    dot <- c(dot, sprintf("  C%s [label=\"%s\", fillcolor=\"%s\", color=\"#666666\"];", cid, node_label, bg_color))
  }
  
  dot <- c(dot, "}")
  paste(dot, collapse = "\n")
}

# --- EXTENDED LATEX EXPORTER: FORMAL CONTEXT TABULAR ---
context_to_latex_tabular <- function(fc) {
  if (is.null(fc)) return("% Empty Formal Context")
  mat <- t(as.matrix(fc$I))
  obj_names <- rownames(mat)
  att_names <- colnames(mat)
  if (is.null(obj_names)) obj_names <- paste0("Obj", 1:nrow(mat))
  if (is.null(att_names)) att_names <- paste0("Att", 1:ncol(mat))
  
  # Escape LaTeX chars helper
  escape_latex <- function(text_vec) {
    if (length(text_vec) == 0) return(character(0))
    # Replace backslashes first, then other LaTeX symbols
    text_vec <- gsub("\\\\", "\\\\backslash", text_vec)
    text_vec <- gsub("([&%$#_{}])", "\\\\\\1", text_vec)
    text_vec <- gsub("~", "\\\\sim{}", text_vec)
    text_vec <- gsub("\\^", "\\\\^{}", text_vec)
    return(text_vec)
  }
  
  obj_names_esc <- escape_latex(obj_names)
  att_names_esc <- escape_latex(att_names)
  
  col_spec <- paste0("|l|", paste(rep("c", length(att_names_esc)), collapse = "|"), "|")
  
  latex_lines <- c(
    "\\begin{table}[h]",
    "\\centering",
    sprintf("\\begin{tabular}{%s}", col_spec),
    "\\hline",
    paste0(paste(c("Objects / Attributes", att_names_esc), collapse = " & "), " \\\\"),
    "\\hline"
  )
  
  for (i in 1:nrow(mat)) {
    row_vals <- ifelse(mat[i, ] > 0, "$\\times$", "")
    row_line <- paste(c(obj_names_esc[i], row_vals), collapse = " & ")
    latex_lines <- c(latex_lines, paste0(row_line, " \\\\"), "\\hline")
  }
  
  latex_lines <- c(
    latex_lines,
    "\\end{tabular}",
    "\\caption{Formal Context Incidence Matrix}",
    "\\label{tab:formal_context}",
    "\\end{table}"
  )
  
  return(paste(latex_lines, collapse = "\n"))
}

# --- EXTENDED LATEX EXPORTER: CONCEPT LATTICE TIKZ GRAPH ---
lattice_to_tikz <- function(concepts, fc = NULL, layout_val = "layout_with_sugiyama", filter_ids = NULL) {
  g_data <- getGraph(concepts, fc, filter_ids = filter_ids)
  nodes <- g_data$nodes
  edges <- g_data$edges
  
  if (nrow(nodes) == 0) {
    return("% Empty Concept Lattice")
  }
  
  # Create an igraph object from the edges
  if (nrow(edges) > 0) {
    g_igraph <- igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
  } else {
    g_igraph <- igraph::make_empty_graph(n = nrow(nodes))
    igraph::V(g_igraph)$name <- nodes$id
  }
  
  # Apply layout matching the selection
  if (layout_val == "layout_with_sugiyama") {
    l <- igraph::layout_with_sugiyama(g_igraph)$layout
  } else {
    l <- igraph::layout_with_kk(g_igraph)
  }
  
  # Normalize layout coordinates to fit nicely in a standard TikZ page (e.g., [0, 12] cm for X, [0, 10] cm for Y)
  x_vals <- l[, 1]
  y_vals <- l[, 2]
  
  min_x <- min(x_vals); max_x <- max(x_vals)
  min_y <- min(y_vals); max_y <- max(y_vals)
  
  if (max_x == min_x) {
    norm_x <- rep(6, length(x_vals))
  } else {
    norm_x <- 12 * (x_vals - min_x) / (max_x - min_x)
  }
  
  if (max_y == min_y) {
    norm_y <- rep(5, length(y_vals))
  } else {
    norm_y <- 10 * (y_vals - min_y) / (max_y - min_y)
  }
  
  tikz_code <- c(
    "\\documentclass[tikz, border=10pt]{standalone}",
    "\\usepackage{tikz}",
    "\\usepackage{xcolor}",
    "",
    "% Define FCA Concept colors",
    "\\definecolor{fcaorange}{RGB}{255,213,128}",
    "\\definecolor{fcablue}{RGB}{151,194,252}",
    "\\definecolor{fcagreen}{RGB}{144,238,144}",
    "\\definecolor{fcagray}{RGB}{224,224,224}",
    "",
    "\\begin{document}",
    "\\begin{tikzpicture}[",
    "  fca_node/.style={circle, draw=black!70, line width=1pt, minimum size=0.8cm, inner sep=2pt, font=\\scriptsize\\sffamily, align=center},",
    "  node_orange/.style={fca_node, fill=fcaorange},",
    "  node_blue/.style={fca_node, fill=fcablue},",
    "  node_green/.style={fca_node, fill=fcagreen},",
    "  node_gray/.style={fca_node, fill=fcagray}",
    "]",
    ""
  )
  
  # Escape LaTeX chars helper for node labels
  escape_latex <- function(text_vec) {
    if (length(text_vec) == 0) return(character(0))
    text_vec <- gsub("([&%$#_{}])", "\\\\\\1", text_vec)
    text_vec <- gsub("~", "\\\\sim{}", text_vec)
    text_vec <- gsub("\\^", "\\\\^{}", text_vec)
    return(text_vec)
  }
  
  # Write Nodes
  tikz_code <- c(tikz_code, "  % Nodes representing FCA Concepts")
  for (i in 1:nrow(nodes)) {
    cid <- nodes$id[i]
    lbl <- nodes$label[i]
    bg_color <- nodes$color.background[i]
    
    # Determine style
    n_style <- "node_gray"
    if (bg_color == "#FFD580") n_style <- "node_orange"
    else if (bg_color == "#97C2FC") n_style <- "node_blue"
    else if (bg_color == "#90EE90") n_style <- "node_green"
    
    # Format label
    esc_lbl <- escape_latex(lbl)
    if (esc_lbl != "") {
      # Replace \n with TikZ line break (\\)
      tikz_lbl <- gsub("\n", " \\\\\\\\ ", esc_lbl)
      node_label <- paste0("C", cid, " \\\\\\\\ ", tikz_lbl)
    } else {
      node_label <- paste0("C", cid)
    }
    
    x_pos <- round(norm_x[i], 2)
    y_pos <- round(norm_y[i], 2)
    
    tikz_code <- c(tikz_code, sprintf("  \\node[%s] (C%s) at (%s, %s) {%s};", n_style, cid, x_pos, y_pos, node_label))
  }
  
  # Write Edges
  tikz_code <- c(tikz_code, "", "  % Edges representing transitive reduction lines")
  if (nrow(edges) > 0) {
    for (j in 1:nrow(edges)) {
      tikz_code <- c(tikz_code, sprintf("  \\draw [->, >=stealth, line width=1pt, draw=black!50] (C%s) -- (C%s);", edges$from[j], edges$to[j]))
    }
  }
  
  tikz_code <- c(tikz_code, "\\end{tikzpicture}", "\\end{document}")
  
  return(paste(tikz_code, collapse = "\n"))
}

get_association_rules <- function(fc, min_support = 0.0, min_confidence = 0.0, selected_concept_id = NULL, method = "covering") {
  if (is.null(fc) || tryCatch(fc$concepts$is_empty(), error = function(e) TRUE)) return(NULL)
  
  exts <- as.matrix(fc$concepts$extents())
  ints <- as.matrix(fc$concepts$intents())
  sizes <- base::colSums(exts)
  N <- length(fc$objects)
  if (is.na(N) || N == 0) N <- 1
  
  rules <- list()
  n_concepts <- fc$concepts$size()
  
  # Calculate transitive reduction to get covering relations
  M <- matrix(0, nrow = n_concepts, ncol = n_concepts)
  for(padre in 1:n_concepts) {
    for(hijo in 1:n_concepts) {
      if(padre != hijo && sizes[padre] > sizes[hijo]) {
        intersection_size <- sum(exts[, hijo] & exts[, padre])
        if(intersection_size == sizes[hijo]) { M[padre, hijo] <- 1 }
      }
    }
  }
  path_len_2 <- (M %*% M) > 0
  M_reduced <- M & (!path_len_2)
  
  if (!is.null(selected_concept_id) && selected_concept_id > 0 && selected_concept_id <= n_concepts) {
    sel_id <- selected_concept_id
    sel_size <- sizes[sel_id]
    
    for (j in 1:n_concepts) {
      if (j != sel_id && sizes[j] <= sel_size) {
        is_related <- if (method == "covering") M_reduced[sel_id, j] == 1 else all(exts[, j] <= exts[, sel_id])
        if (is_related) {
          support <- sizes[j] / N
          confidence <- if (sel_size > 0) sizes[j] / sel_size else 0
          
          if (support >= min_support && confidence >= min_confidence) {
            lhs_atts <- fc$attributes[which(ints[, sel_id] == 1)]
            rhs_atts <- fc$attributes[which(ints[, j] == 1 & ints[, sel_id] == 0)]
            if (length(rhs_atts) > 0) {
              rules[[length(rules) + 1]] <- list(
                LHS = paste(lhs_atts, collapse = ", "),
                RHS = paste(rhs_atts, collapse = ", "),
                Support = support,
                Confidence = confidence,
                Parent_ID = sel_id,
                Child_ID = j
              )
            }
          }
        }
      }
    }
    
    for (i in 1:n_concepts) {
      if (i != sel_id && sizes[i] >= sel_size) {
        is_related <- if (method == "covering") M_reduced[i, sel_id] == 1 else all(exts[, sel_id] <= exts[, i])
        if (is_related) {
          support <- sel_size / N
          confidence <- if (sizes[i] > 0) sel_size / sizes[i] else 0
          
          if (support >= min_support && confidence >= min_confidence) {
            lhs_atts <- fc$attributes[which(ints[, i] == 1)]
            rhs_atts <- fc$attributes[which(ints[, sel_id] == 1 & ints[, i] == 0)]
            if (length(rhs_atts) > 0) {
              rules[[length(rules) + 1]] <- list(
                LHS = paste(lhs_atts, collapse = ", "),
                RHS = paste(rhs_atts, collapse = ", "),
                Support = support,
                Confidence = confidence,
                Parent_ID = i,
                Child_ID = sel_id
              )
            }
          }
        }
      }
    }
  } else {
    if (method == "covering") {
      edges <- which(M_reduced == 1, arr.ind = TRUE)
      if (nrow(edges) > 0) {
        for (idx in 1:nrow(edges)) {
          i <- edges[idx, 1]
          j <- edges[idx, 2]
          size_i <- sizes[i]
          size_j <- sizes[j]
          
          support <- size_j / N
          confidence <- if (size_i > 0) size_j / size_i else 0
          
          if (support >= min_support && confidence >= min_confidence) {
            lhs_atts <- fc$attributes[which(ints[, i] == 1)]
            rhs_atts <- fc$attributes[which(ints[, j] == 1 & ints[, i] == 0)]
            if (length(rhs_atts) > 0) {
              rules[[length(rules) + 1]] <- list(
                LHS = paste(lhs_atts, collapse = ", "),
                RHS = paste(rhs_atts, collapse = ", "),
                Support = support,
                Confidence = confidence,
                Parent_ID = i,
                Child_ID = j
              )
            }
          }
        }
      }
    } else {
      candidate_ids <- which((sizes / N) >= min_support)
      ordered_ids <- candidate_ids[order(sizes[candidate_ids], decreasing = TRUE)]
      n_ord <- length(ordered_ids)
      if (n_ord > 1) {
        for (idx_i in 1:(n_ord - 1)) {
          i <- ordered_ids[idx_i]
          size_i <- sizes[i]
          for (idx_j in (idx_i + 1):n_ord) {
            j <- ordered_ids[idx_j]
            size_j <- sizes[j]
            
            if (all(exts[, j] <= exts[, i])) {
              support <- size_j / N
              confidence <- if (size_i > 0) size_j / size_i else 0
              
              if (support >= min_support && confidence >= min_confidence) {
                lhs_atts <- fc$attributes[which(ints[, i] == 1)]
                rhs_atts <- fc$attributes[which(ints[, j] == 1 & ints[, i] == 0)]
                if (length(rhs_atts) > 0) {
                  rules[[length(rules) + 1]] <- list(
                    LHS = paste(lhs_atts, collapse = ", "),
                    RHS = paste(rhs_atts, collapse = ", "),
                    Support = support,
                    Confidence = confidence,
                    Parent_ID = i,
                    Child_ID = j
                  )
                  if (length(rules) >= 5000) break
                }
              }
            }
          }
          if (length(rules) >= 5000) break
        }
      }
    }
  }
  
  if (length(rules) == 0) return(data.frame(LHS = character(0), RHS = character(0), Support = numeric(0), Confidence = numeric(0), Parent_ID = integer(0), Child_ID = integer(0)))
  
  df <- do.call(rbind, lapply(rules, as.data.frame))
  return(df)
}


