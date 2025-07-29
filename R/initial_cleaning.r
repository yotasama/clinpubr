#' Preliminarily cleaning string vectors
#' @description Cleaning illegal characters in string vectors that store numerical values.
#'   The function is useful for cleaning electrical health records in Chinese.
#'
#'   `char_initial_cleaning()` will convert full-width characters to half-width characters,
#'   removes whitespace at the start and end, replaces all internal whitespace with a single space,
#'   and replace empty strings with `NA`.
#'
#'   `value_initial_cleaning()` will additionally remove all spaces and extra dots.
#' @param x A string vector.
#' @returns A string vector with less illegal characters.
#' @export
#' @examples
#' x <- c("\uFF11\uFF12\uFF13", "11..23", "\uff41\uff42\uff41\uff4e\uff44\uff4f\uff4e", 
#'        "hello world ")
#' value_initial_cleaning(x)
#' char_initial_cleaning(x)
value_initial_cleaning <- function(x) {
  x <- stringi::stri_trans_general(x, "Fullwidth-Halfwidth")
  x <- str_squish(x)
  x <- str_replace_all(x, c(" " = "", "\\.+" = "\\."))
  x[which(x == "")] <- NA
  x
}


#' @rdname value_initial_cleaning
#' @export
char_initial_cleaning <- function(x) {
  x <- stringi::stri_trans_general(x, "Fullwidth-Halfwidth")
  x <- str_squish(x)
  x[which(x == "")] <- NA
  x
}