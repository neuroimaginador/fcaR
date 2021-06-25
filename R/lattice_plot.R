lattice_plot <- function(extents, intents,
                         subconcept_matrix,
                         objects, attributes,
                         object_names,
                         to_latex, ...) {

  # Number of concepts
  n <- ncol(extents)

  if (to_latex) {

    if (object_names) {

      labels <- sapply(seq(n),
                       function(i) {

                         vA <- Matrix::Matrix(extents[, i],
                                              sparse = TRUE)
                         vB <- Matrix::Matrix(intents[, i],
                                              sparse = TRUE)

                         vA <- Set$new(attributes = objects,
                                       M = vA)
                         vB <- Set$new(attributes = attributes,
                                       M = vB)

                         paste0("$\\left(\\,",
                                vA$to_latex(print = FALSE),
                                ",\\right.",
                                "\\left.",
                                vB$to_latex(print = FALSE),
                                "\\,\\right)$") %>%
                           stringr::str_replace_all(pattern = "\n",
                                                    replacement = "")

                       })

    } else {

      labels <- sapply(seq(n),
                       function(i) {

                         vB <- Matrix::Matrix(intents[, i],
                                              sparse = TRUE)

                         vB <- Set$new(attributes = attributes,
                                       M = vB)

                         vB$to_latex(print = FALSE) %>%
                           stringr::str_replace_all(pattern = "\n",
                                                    replacement = "")

                       })

    }

    labels <- labels %>%
      stringr::str_replace_all(pattern = stringr::fixed(" "),
                               replacement = "\\,")

  } else {

    if (object_names) {

      labels <- sapply(seq(n),
                       function(i) {

                         vA <- Matrix::Matrix(extents[, i],
                                              sparse = TRUE)
                         vB <- Matrix::Matrix(intents[, i],
                                              sparse = TRUE)

                         .concept_to_string(vA, vB,
                                            objects,
                                            attributes)

                       })

    } else {

      labels <- sapply(seq(n),
                       function(i) {

                         vB <- Matrix::Matrix(intents[, i],
                                              sparse = TRUE)

                         .set_to_string(vB,
                                        attributes)

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
      "\\usepackage{tikz}",
      "\\usepackage[active,tightpage,psfixbb]{preview}",
      "\\PreviewEnvironment{pgfpicture}",
      "\\setlength\\PreviewBorder{0pt}",
      # getOption( "tikzLatexPackages" ),
      "\\usepackage{amssymb}"
    ))

    args[names(dots)] <- dots[names(dots)]

    on.exit({
      if (!is.null(grDevices::dev.list()))
        grDevices::dev.off()
    })
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
