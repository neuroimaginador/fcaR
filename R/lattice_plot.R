#' @title Plot Concept Lattice
#' @description Visualization of the concept lattice using 'ggraph'.
#'
#' @param nodes_df Data frame with 'id'.
#' @param cover_matrix Sparse matrix.
#' @param method Layout method ("sugiyama", "force").
#' @param mode Labeling mode ("reduced", "full", "empty").
#' @param objects Character vector.
#' @param attributes Character vector.
#' @param object_names Logical (Deprecated).
#' @param to_latex Logical.
#' @param extents List of extents.
#' @param intents List of intents.
#' @param viewer The viewer to use for plotting: "ggraph" (default) or "base" (experimental, R base graphics).
#' @param theme The theme to use for the "base" viewer ("standard", "nord", "latex", "vibrant").
#' @param ... Extra args.
#'
#' @export
lattice_plot <- function(
  nodes_df,
  cover_matrix,
  method = "sugiyama",
  mode = NULL,
  objects = NULL,
  attributes = NULL,
  object_names = TRUE,
  to_latex = FALSE,
  extents = NULL,
  intents = NULL,
  viewer = c("ggraph", "base"),
  theme = "standard",
  ...
) {
  viewer <- match.arg(viewer)
  if (is.null(cover_matrix)) {
    stop("Covering matrix is missing.")
  }

  # 1. Heurística
  if (is.null(mode)) {
    if (nrow(nodes_df) > 50) mode <- "empty" else mode <- "reduced"
  }

  # 2. Datos Base
  cover_edges_df <- sparse_matrix_to_edges(cover_matrix)

  if (!"grade" %in% colnames(nodes_df)) {
    grades_vec <- calculate_grades(
      nodes_df$id,
      cover_edges_df$from,
      cover_edges_df$to
    )
    nodes_df$grade <- grades_vec[as.character(nodes_df$id)]
  }

  # 2.5 Grade Balancing (only for Sugiyama)
  if (method == "sugiyama") {
    nodes_df$balanced_grade <- as.numeric(balance_grades(
      nodes_df$grade,
      nodes_df$id,
      cover_edges_df$from,
      cover_edges_df$to
    ))
  } else {
    nodes_df$balanced_grade <- as.numeric(nodes_df$grade)
  }

  # 3. Layout (C++)
  layout_df <- calculate_lattice_layout_rcpp(
    concept_ids = nodes_df$id,
    layers_vec = as.integer(nodes_df$grade),
    y_coords_vec = as.numeric(nodes_df$balanced_grade),
    edge_from = cover_edges_df$from,
    edge_to = cover_edges_df$to,
    method = method
  )

  # Vertical alignment refinement for single-node layers (Sugiyama)
  if (method == "sugiyama") {

    predecessors <- split(as.character(cover_edges_df$from),
                          as.character(cover_edges_df$to))

    # Refine X for single-node layers to align with parent
    layers <- split(as.character(nodes_df$id),
                    as.character(nodes_df$grade))

    for (g in names(layers)) {
      if (length(layers[[g]]) == 1) {
        id <- layers[[g]]
        preds <- predecessors[[id]]
        if (length(preds) == 1) {
          # Align with the only predecessor
          layout_df$x[layout_df$id == id] <- layout_df$x[layout_df$id == preds]
        }
      }
    }

  }

  plot_data <- merge(nodes_df, layout_df, by = "id")

  # 4. Etiquetas y Colores
  # Si faltan datos, forzamos modo empty para no romper
  if (mode != "empty" && (is.null(extents) || is.null(intents))) {
    warning("Missing extents/intents for labels. Using 'empty' mode.")
    mode <- "empty"
  }

  if (isTRUE(to_latex)) {
    objects <- sapply(objects, format_label)
    attributes <- sapply(attributes, format_label)
  }

  plot_data <- compute_labels_and_colors(
    nodes_df = plot_data,
    cover_edges = cover_edges_df,
    extents = extents,
    intents = intents,
    obj_names = objects,
    att_names = attributes,
    mode = mode
  )

  if (isTRUE(to_latex)) {
    # Llamamos a la nueva función exportadora
    # Podemos ajustar la escala según el tamaño del retículo
    # scale_factor <- if (nrow(plot_data) > 20) 1.5 else 2.0

    return(export_to_tikz(plot_data, cover_edges_df))
  }

  if (viewer == "base") {

    return(plot_lattice_base(
      plot_data,
      cover_edges_df,
      theme = theme,
      ...
    ))

  } else {

    check_needed_pkg("ggraph", "plotting the lattice with ggraph viewer")

    g <- igraph::graph_from_data_frame(
      cover_edges_df,
      vertices = plot_data,
      directed = TRUE
    )

    # 5. Construcción del Gráfico
    p <- ggraph::ggraph(g, layout = "manual", x = x, y = y) +
      # Aristas
      ggraph::geom_edge_fan(color = "gray70", alpha = 0.6) +

      # Nodos: Ahora 'fill_color' siempre existe (incluso "white" en empty)
      ggraph::geom_node_point(
        ggplot2::aes(fill = fill_color),
        size = 5,
        shape = 21,
        color = "gray30", # Borde gris oscuro
        stroke = 0.8
      ) +

      ggplot2::scale_fill_identity() +
      ggplot2::theme_void()

    # Añadir texto SOLO si no es empty y hay etiquetas
    if (mode != "empty") {
      p <- p +
        ggraph::geom_node_text(
          ggplot2::aes(label = label_top),
          repel = TRUE,
          vjust = -1.2,
          size = 3,
          color = "black",
          bg.color = "white",
          bg.r = 0.15,
          na.rm = TRUE
        ) +
        ggraph::geom_node_text(
          ggplot2::aes(label = label_bottom),
          repel = TRUE,
          vjust = 2.2,
          size = 3,
          color = "#0055AA",
          fontface = "italic",
          bg.color = "white",
          bg.r = 0.15,
          na.rm = TRUE
        )
    }

    print(p)
    return(invisible(p))
  }
}

# @title Plot Concept Lattice
# @description Visualization of the concept lattice using 'ggraph'.
# Supports hierarchical (Freese) and force-directed layouts.
#
# @param nodes_df Data frame with 'id' column.
# @param cover_matrix Sparse matrix (covering relation).
# @param method Character. Layout algorithm: "sugiyama" (default, layered/hierarchical) or "force" (spring-based).
# @param objects Character vector of object names.
# @param attributes Character vector of attribute names.
# @param object_names Logical. Show object names?
# @param to_latex Logical. Export to LaTeX?
# @param ... Arguments passed to internal plotting.
#
#' @keywords internal
lattice_plot_legacy <- function(...) {
  # This function is kept for backward compatibility if needed, 
  # but redirected to the new lattice_plot
  lattice_plot(...)
}

#' @title Balance Grades for Visual Symmetry
#' @description Adjusts the vertical position of nodes to be centered between their
#'   predecessors and successors. This improves the visual appearance of lattices
#'   like the pentagon (N5).
#'
#' @param grades A named numeric vector of initial grades.
#' @param ids A vector of element IDs.
#' @param edge_from Source IDs of the cover edges.
#' @param edge_to Target IDs of the cover edges.
#'
#' @return A named numeric vector of balanced grades.
#' @noRd
balance_grades <- function(grades, ids, edge_from, edge_to) {
  y <- as.numeric(grades)
  names(y) <- as.character(ids)

  predecessors <- split(as.character(edge_from), as.character(edge_to))
  successors <- split(as.character(edge_to), as.character(edge_from))

  # Heuristic: if a node has exactly one predecessor and one successor,
  # place it in the middle of them. Repeat a few times.
  for (iter in 1:5) {
    new_y <- y
    for (id in names(y)) {
      preds <- predecessors[[id]]
      succs <- successors[[id]]

      if (length(preds) == 1 && length(succs) == 1) {
        new_y[id] <- (y[preds] + y[succs]) / 2
      }
    }
    if (max(abs(new_y - y)) < 0.01) break
    y <- new_y
  }

  return(y)
}

#' @title Draw Hasse Diagram using Base R
#' @description Renders a poset diagram using base graphics.
#'
#' @param plot_data Data frame with id, x, y, label_top, label_bottom, fill_color.
#' @param edges_df Data frame with from, to columns.
#' @param theme The theme to use: "standard", "nord", "latex", "vibrant".
#' @noRd
plot_lattice_base <- function(plot_data, edges_df, theme = "standard", ...) {
  # Define color palettes
  palettes <- list(
    standard = list(node_fill = "white", node_border = "gray30", edge_col = "gray70", bg = "white", text_col = "black"),
    nord = list(node_fill = "#88C0D0", node_border = "#2E3440", edge_col = "#4C566A", bg = "#ECEFF4", text_col = "#2E3440"),
    latex = list(node_fill = "white", node_border = "black", edge_col = "black", bg = "white", text_col = "black"),
    vibrant = list(node_fill = "#FF4081", node_border = "#303F9F", edge_col = "#7B1FA2", bg = "#F5F5F5", text_col = "#303F9F")
  )

  pal <- palettes[[theme]]
  if (is.null(pal)) pal <- palettes$standard

  # Margins and plot setup
  old_par <- graphics::par(mar = c(1, 1, 1, 1), bg = pal$bg)
  on.exit(graphics::par(old_par))

  # Determine plot ranges
  rx <- range(plot_data$x)
  ry <- range(plot_data$y)

  # Scale X coordinates
  dx <- diff(rx)
  dy <- diff(ry)
  if (dy == 0) dy <- 1
  if (dx == 0) dx <- 1

  target_width <- dy * 0.6
  if (dx < target_width) {
    scale_factor <- target_width / dx
    plot_data$x <- plot_data$x * scale_factor
    rx <- range(plot_data$x)
  }

  xlim <- c(rx[1] - 0.8, rx[2] + 0.8)
  ylim <- c(ry[1] - 0.5, ry[2] + 0.5)

  # Initialize plot
  graphics::plot(NA, NA, xlim = xlim, ylim = ylim,
       axes = FALSE, xlab = "", ylab = "", ...)

  # Draw edges
  for (i in seq_len(nrow(edges_df))) {
    u <- plot_data[plot_data$id == edges_df$from[i], ]
    v <- plot_data[plot_data$id == edges_df$to[i], ]
    if (nrow(u) == 1 && nrow(v) == 1) {
      graphics::segments(u$x, u$y, v$x, v$y, col = pal$edge_col, lwd = 1.2)
    }
  }

  # Draw nodes
  # Use fill_color if provided in plot_data, otherwise pal$node_fill
  node_fills <- if ("fill_color" %in% colnames(plot_data)) plot_data$fill_color else rep(pal$node_fill, nrow(plot_data))

  graphics::points(plot_data$x, plot_data$y,
         pch = 21, bg = node_fills,
         col = pal$node_border, cex = 2.5, lwd = 1.2)

  # Draw labels
  text_font <- if (theme == "latex") "serif" else "sans"

  # Top labels (Attributes in FCA)
  if ("label_top" %in% colnames(plot_data)) {
    graphics::text(plot_data$x, plot_data$y + 0.25,
         labels = plot_data$label_top,
         cex = 0.7, font = 3, col = if(theme == "latex") "black" else "blue", family = text_font)
  }

  # Bottom labels (Objects in FCA)
  if ("label_bottom" %in% colnames(plot_data)) {
    graphics::text(plot_data$x, plot_data$y - 0.25,
         labels = plot_data$label_bottom,
         cex = 0.7, font = 3, col = if(theme == "latex") "black" else "darkgreen", family = text_font)
  }

  return(invisible(NULL))
}
