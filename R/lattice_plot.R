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
  ...
) {
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

  # 3. Layout (C++)
  layout_df <- calculate_lattice_layout_rcpp(
    concept_ids = nodes_df$id,
    grades = nodes_df$grade,
    edge_from = cover_edges_df$from,
    edge_to = cover_edges_df$to,
    method = method
  )
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
  } else {
    check_needed_pkg("ggraph", "plotting the lattice")

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
# @keywords internal
lattice_plot_legacy <- function(
  nodes_df,
  cover_matrix,
  method = "sugiyama", # Nuevo parámetro
  objects = NULL,
  attributes = NULL,
  object_names = TRUE,
  to_latex = FALSE,
  ...
) {
  # if (is.null(cover_matrix)) stop("Covering matrix is missing.")
  #
  # # Edge prep
  # cover_edges_df <- sparse_matrix_to_edges(cover_matrix)
  #
  # # Grade prep (Siempre útil, incluso para Force como punto de partida o color)
  # if (!"grade" %in% colnames(nodes_df)) {
  #   grades_vec <- calculate_grades(
  #     concept_ids = nodes_df$id,
  #     edge_from = cover_edges_df$from,
  #     edge_to = cover_edges_df$to
  #   )
  #   nodes_df$grade <- grades_vec[as.character(nodes_df$id)]
  # }
  #
  # # Layout Calculation (C++ Dispatch)
  # # Pasamos el método elegido ("freese" o "force")
  # layout_df <- calculate_lattice_layout_rcpp(
  #   concept_ids = nodes_df$id,
  #   grades = nodes_df$grade,
  #   edge_from = cover_edges_df$from,
  #   edge_to = cover_edges_df$to,
  #   method = method
  # )
  #
  # plot_data <- merge(nodes_df, layout_df, by = "id")
  # plot_data$label <- as.character(plot_data$id)
  #
  # if (isTRUE(to_latex)) {
  #   return(list(nodes = plot_data, edges = cover_edges_df))
  # } else {
  #   if (!requireNamespace("ggraph", quietly = TRUE)) stop("Install 'ggraph'.")
  #
  #   g <- igraph::graph_from_data_frame(cover_edges_df, vertices = plot_data, directed = TRUE)
  #
  #   # El layout es manual porque C++ ya calculó x, y
  #   p <- ggraph::ggraph(g, layout = "manual", x = x, y = y) +
  #     ggraph::geom_edge_fan(color = "gray60", alpha = 0.6) +
  #     ggraph::geom_node_point(size = 4, shape = 21, fill = "white", color = "black") +
  #     ggraph::geom_node_text(ggplot2::aes(label = label), repel = TRUE, size = 3) +
  #     ggplot2::theme_void()
  #
  #   print(p)
  #   return(invisible(p))
  # }
}
