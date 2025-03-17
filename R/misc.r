#' Generate code from string vector
#' Genearte the code that can be used to generate the string vector.
#' @param x A string vector.
#'
#' @returns A string that contains the code to generate the vector.
#' @export
#' @examples
#' vec2code(colnames(mtcars))
vec2code <- function(x) {
  paste0("c('", paste0(x, collapse = "','"), "')")
}


#' Format p-value for publication
#' @param p The numerical p values to be formated.
#'
#' @returns A string vector of formated p values.
#' @export
#' @examples
#' format_pval(c(0.001, 0.0001, 0.05, 0.1123456))
format_pval <- function(p) {
  base::format.pval(p, digits = 1, nsmall = 2, eps = 1e-3)
}

#' @importFrom DescTools Mode
NULL
#' Calculate the first mode
#' Calculate the first mode of a vector. Ignore NA values.
#'   Can be used if any mode is acceptable.
#' @param x A vector.
#'
#' @returns The first mode of the vector.
#' @export
#' @examples
#' first_mode(c(1, 1, 2, 2, 3, 3, 3, NA, NA, NA))
first_mode <- function(x) {
  x <- na.omit(x)
  l <- length(unique(x))
  if (l == 0) {
    NA
  } else if (l == 1 | l == length(x)) {
    x[1]
  } else {
    Mode(x)[1]
  }
}
