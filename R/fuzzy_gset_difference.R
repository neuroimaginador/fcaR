fuzzy_gset_difference <- function(...) {
  l <- list(...)
  CON <- function(x, y) { x[x <= y] <- 0; return(x)}
  support <- as.set(l[[1]])
  if (set_is_empty(support))
    return(set())
  sets:::.make_gset_from_list_of_gsets_and_support_and_connector(l,
                                                          support,
                                                          CON)
}
