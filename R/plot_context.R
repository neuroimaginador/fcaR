#' @importFrom rlang .data
plot_context <- function(I, to_latex, ...) {

  if (to_latex) {

    check_needed_pkg("tikzDevice", "exporting plots to LaTeX/TikZ")

    tmp_file <- tempfile(fileext = ".tex")
    dots <- list(...)
    args <- list(file = tmp_file,
                 standAlone = FALSE,
                 sanitize = TRUE,
                 width = 4,
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

    do.call(tikzDevice::tikz, args = args)

  }

  color_function <- function(s)
    grDevices::rgb(red = 1 - s,
                   green = 1 - s,
                   blue = 1 - s)
  # stats::heatmap(t(Matrix::as.matrix(I)), Rowv = NA, Colv = NA,
  #         col = color_function(seq(0, 1, 0.01)),
  #         scale = "none")

  # Heatmap
  p <- I |>
    Matrix::as.matrix() |> t() |>
    # Data wrangling
    as.data.frame() |>
    tibble::rownames_to_column(var = "object") |>
    tidyr::pivot_longer(cols = rownames(I)) |>
    dplyr::mutate(text = glue::glue(
      "{object}, {name} is {value}"
    )) |>
    # Viz
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$name,
        y = .data$object,
        fill = .data$value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(
      low = "white",
      high = "black") +
    ggplot2::theme_minimal() +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_y_discrete(
      limits = rev(colnames(I))
    ) +
    ggplot2::scale_x_discrete(
      limits = rownames(I),
      position = "top"
    )

  print(p)

  # plotly::ggplotly(p, tooltip = "text")

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
