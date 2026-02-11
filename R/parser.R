#' Parses several implications given as a string
#'
#' @param input (character) The string with the implications or a file containing the implications
#'
#' @details
#' The format for the input file is:
#'
#'  - Every implication in its own line or separated by semicolon (;)
#'  - Attributes are separated by commas (,)
#'  - The LHS and RHS of each implication are separated by an arrow (->)
#'
#' @export
#'
#' @return An ImplicationSet
#'
#' @importFrom stringr str_split str_squish
#' @importFrom purrr map pluck
#'
#' @examples
#' input <- system.file("implications", "ex_implications", package = "fcaR")
#' imps <- parse_implications(input)
#'
parse_implications <- function(input) {

  # Check if input is a filename
  if (file.exists(input)) {

    input <- readLines(input)

  }

  if (!any(stringr::str_detect(input, "->"))) {

    stop("The input is neither a file (or does not contain the full path of a file), nor an implication.", call. = FALSE)

  }

  # Detect attributes
  attributes <- input |>
    stringr::str_split("(,|->|;)") |>
    purrr::map(stringr::str_squish) |>
    unlist() |> unique()

  # Split implications and obtain LHS and RHS matrices
  LR <- input |>
    stringr::str_split(";") |> unlist() |>
    purrr::map(function(x) parse_implication(x, attributes))

  # Combine matrices of each implication
  LHS <- LR |>
    purrr::map(purrr::pluck("lhs"))

  LHS <- do.call(cbind, LHS)

  RHS <- LR |>
    purrr::map(purrr::pluck("rhs"))

  RHS <- do.call(cbind, RHS)

  # Build the ImplicationSet
  ImplicationSet$new(attributes = attributes,
                     lhs = LHS,
                     rhs = RHS)

}

#' Parses a string into an implication
#'
#' @param string      (character) The string to be parsed
#' @param attributes  (character vector) The attributes' names
#'
#' @return Two vectors as sparse matrices representing the LHS and RHS of the implication
#'
#' @importFrom stringr str_split fixed str_replace_all str_which
#' @importFrom purrr map
#' @importFrom Matrix Matrix
parse_implication <- function(string, attributes) {

  # Split left and right hand sides
  LR <- string |>
    stringr::str_split(pattern = stringr::fixed("->")) |>
    purrr::map(stringr::str_squish)

  LR <- LR[[1]]

  # Add some markers as delimiters for attributes
  LR <- LR |>
    stringr::str_replace_all(pattern = "\\s*,\\s*",
                             replacement = "%%%")

  LR <- paste0("%%%", LR, "%%%")

  # Index of the attributes found in the string
  idx <- LR |>
    purrr::map(function(s)
      stringr::str_which(s,
                         pattern = paste0("%%%",
                                          attributes,
                                          "%%%")))

  # LHS and RHS matrices
  LHS <- RHS <- Matrix::Matrix(0, ncol = 1, nrow = length(attributes), sparse = TRUE)

  LHS[idx[[1]], 1] <- 1
  RHS[idx[[2]], 1] <- 1

  return(list(lhs = LHS, rhs = RHS))

}
