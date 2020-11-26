reorder_layout <- function(layout,
                           labels = rep(" ", nrow(layout))) {

  widths <- grid::convertX(grid::stringWidth(labels),
                           unitTo = "points",
                           valueOnly = TRUE)
  height <- layout[, 2]
  x <- layout[, 1]

  max_width <- split(widths, height) %>% sapply(sum) %>% max()
  available_x <- c(0, max_width * 1.2)

  result <- layout

  for (i in sort(unique(height))) {

    ids <- which(height == i)
    o <- order(x[ids])

    w <- widths[ids]
    w <- w[o]
    sw <- sum(w)
    new_x <- c()
    for (i in seq(0, length(w) - 1)) {

      if (i > 0) {

        new_x <- c(new_x, sum(w[seq(i)]) + w[i + 1]/2)

      } else {

        new_x <- w[i + 1] / 2

      }

    }

    new_x <- new_x * available_x[2] / sw

    # new_x <- seq(from = available_x[1],
    #              to = available_x[2],
    #              length.out = length(ids) + 2)
    # new_x <- new_x[-c(1, length(new_x))]

    result[ids, 1] <- new_x[o]

  }

  return(result)

}
