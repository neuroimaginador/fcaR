lattice_plot <- function(concepts, subconcept_matrix,
                         objects, attributes,
                         object_names,
                         to_latex, ...) {

  if (to_latex) {

    if (object_names) {

      labels <- sapply(concepts,
                       function(l) l$to_latex(print = FALSE)) %>%
        stringr::str_replace_all(pattern = "\n",
                                 replacement = "")

    } else {

      labels <- sapply(concepts,
                       function(l) {
                         v <- l$get_intent()
                         v$to_latex(print = FALSE)
                       })

    }

    labels <- labels %>%
      stringr::str_replace_all(pattern = stringr::fixed(" "),
                               replacement = "\\,")

  } else {

    if (object_names) {

      labels <- sapply(concepts,
                       function(l) .concept_to_string(l,
                                                      objects,
                                                      attributes))

    } else {

      labels <- sapply(concepts,
                       function(l) {
                         v <- l$get_intent()
                         .set_to_string(S = v$get_vector(),
                                        attributes = v$get_attributes())
                       })

    }

  }

  if (to_latex) {

    tmp_file <- tempfile(fileext = ".tex")
    dots <- list(...)
    args <- list(file = tmp_file,
                 standAlone = FALSE,
                 sanitize = FALSE,
                 width = 6,
                 height = 4)

    if ("filename" %in% names(dots)) {

      filename <- dots$filename
      dots$filename <- NULL

    } else {

      filename <- tempfile(fileext = ".tex")

    }

    if ("caption" %in% names(dots)) {

      caption <- dots$caption
      dots["caption"] <- NULL
      label <- dots$label
      if (is.null(label)) {

        label <- "fig:"

      } else {

        dots["label"] <- NULL

      }

      caption <- paste0("\\label{",
                        label,
                        "}",
                        caption)

      tex_prefix <- c("\\begin{figure}",
                      "\\centering",
                      "")

      tex_suffix <- c("",
                      paste0("\\caption{", caption, "}"),
                      "",
                      "\\end{figure}")

    } else {

      tex_prefix <- c()
      tex_suffix <- c()

    }

    old_opt <- getOption("tikzDocumentDeclaration")

    if ("pointsize" %in% names(dots)) {

      options("tikzDocumentDeclaration" = paste0("\\documentclass[", dots$pointsize,
                                                 "pt]{article}\n"))

    }

    options( tikzLatexPackages = c(
      getOption( "tikzLatexPackages" ),
      "\\usepackage{amssymb}"
    ))

    args[names(dots)] <- dots[names(dots)]

    do.call(tikzDevice::tikz, args = args)

  }

  # MM <- private$subconcept_matrix %>%
  #   .reduce_transitivity()
  #
  # colnames(MM) <- rownames(MM) <- labels
  # g <- igraph::graph_from_adjacency_matrix(adjmatrix = as.matrix(MM))
  #
  # tree_layout <- igraph::layout_as_tree(g, root = 1, mode = "in") %>%
  #   reorder_layout(labels = labels)
  #
  # # print(tree_layout)
  #
  #
  # p <- ggplot2::ggplot(
  #   ggnetwork::ggnetwork(as.matrix(Matrix::t(MM)),
  #             layout = tree_layout),
  #   aes(x = x, y = y, xend = xend, yend = yend)) +
  #   ggnetwork::geom_edges(color = "grey50", arrow = arrow(length = unit(6, "points"))) +
  #   ggnetwork::geom_nodelabel(aes(label = vertex.names)) +
  #   theme_blank()
  #
  # print(p)

  hasseDiagram::hasse(data = Matrix::as.matrix(Matrix::t(subconcept_matrix)),
                      labels = labels,
                      parameters = list(arrows = "backward"))

  if (to_latex) {

    grDevices::dev.off()

    tex <- readLines(tmp_file)
    unlink(tmp_file)

    tex <- c(tex_prefix,
             tex,
             tex_suffix)

    options("tikzDocumentDeclaration" = old_opt)
    my_tex <- paste0(tex, collapse = "\n")
    cat(my_tex, file = filename)

    return(filename)

  }

}
