#' @title Compute Labels and Colors for Lattice Nodes
#' @description Internal function to calculate node labels based on the selected mode.
#'
#' @param nodes_df Data frame with 'id'.
#' @param cover_edges Data frame with 'from', 'to'.
#' @param extents List of numeric vectors.
#' @param intents List of numeric vectors.
#' @param obj_names Character vector.
#' @param att_names Character vector.
#' @param mode Character: "full", "attributes", "reduced", "empty".
#'
#' @return A data frame extending nodes_df with labels and colors.
compute_labels_and_colors <- function(nodes_df, cover_edges, extents, intents, obj_names, att_names, mode) {
  n <- nrow(nodes_df)

  # 1. INICIALIZACIÓN SEGURA (Siempre crear las columnas)
  nodes_df$label_top <- NA_character_
  nodes_df$label_bottom <- NA_character_
  # Color por defecto: Blanco (para que en modo "empty" se vean los nodos)
  nodes_df$fill_color <- "white"

  # 2. MODO EMPTY: Salir ahora, pero con el data.frame bien formado
  if (mode == "empty") {
    return(nodes_df)
  }

  # Pre-procesamiento de nombres
  if (is.null(obj_names)) obj_names <- paste0("Obj", seq_len(max(unlist(extents), 0) + 1))
  if (is.null(att_names)) att_names <- paste0("Att", seq_len(max(unlist(intents), 0) + 1))

  # --- MODO REDUCED ---
  if (mode == "reduced") {
    # Attribute Concepts (Top)
    for (i in seq_len(n)) {
      idx <- nodes_df$id[i]
      current_intent <- intents[[idx]]
      if (length(current_intent) == 0) next

      parents <- cover_edges$to[cover_edges$from == idx]
      if (length(parents) == 0) {
        attribs_to_label <- current_intent
      } else {
        parent_intents <- unique(unlist(lapply(parents, function(p) intents[[p]])))
        attribs_to_label <- setdiff(current_intent, parent_intents)
      }

      if (length(attribs_to_label) > 0) {
        nodes_df$label_top[i] <- paste(att_names[attribs_to_label], collapse = ",\n")
      }
    }

    # Object Concepts (Bottom)
    for (i in seq_len(n)) {
      idx <- nodes_df$id[i]
      current_extent <- extents[[idx]]
      if (length(current_extent) == 0) next

      children <- cover_edges$from[cover_edges$to == idx]
      if (length(children) == 0) {
        objects_to_label <- current_extent
      } else {
        child_extents <- unique(unlist(lapply(children, function(c) extents[[c]])))
        objects_to_label <- setdiff(current_extent, child_extents)
      }

      if (length(objects_to_label) > 0) {
        nodes_df$label_bottom[i] <- paste(obj_names[objects_to_label], collapse = ",\n")
      }
    }

    # Coloreado Lógico
    has_att <- !is.na(nodes_df$label_top)
    has_obj <- !is.na(nodes_df$label_bottom)

    nodes_df$fill_color[has_att & !has_obj] <- "black"
    nodes_df$fill_color[!has_att & has_obj] <- "#619CFF" # Azul
    nodes_df$fill_color[has_att & has_obj] <- "gray50"
  }

  # --- MODO ATTRIBUTES ---
  else if (mode == "attributes") {
    for (i in seq_len(n)) {
      idx <- nodes_df$id[i]
      atts <- intents[[idx]]
      if (length(atts) > 0) {
        txt <- paste(att_names[atts], collapse = ", ")
        nodes_df$label_top[i] <- paste(strwrap(txt, width = 20), collapse = "\n")
      }
    }
    nodes_df$fill_color[!is.na(nodes_df$label_top)] <- "black"
  }

  # --- MODO FULL ---
  else if (mode == "full") {
    for (i in seq_len(n)) {
      idx <- nodes_df$id[i]

      # Intent
      atts <- intents[[idx]]
      if (length(atts) > 0) {
        txt <- paste(att_names[atts], collapse = ", ")
        nodes_df$label_top[i] <- paste(strwrap(txt, width = 20), collapse = "\n")
      }

      # Extent
      objs <- extents[[idx]]
      if (length(objs) > 0) {
        txt <- paste(obj_names[objs], collapse = ", ")
        nodes_df$label_bottom[i] <- paste(strwrap(txt, width = 20), collapse = "\n")
      }
    }
    # Si tiene algo de info, negro. Si es el concepto vacio/universal puro, blanco.
    has_info <- !is.na(nodes_df$label_top) | !is.na(nodes_df$label_bottom)
    nodes_df$fill_color[has_info] <- "black"
  }

  return(nodes_df)
}
