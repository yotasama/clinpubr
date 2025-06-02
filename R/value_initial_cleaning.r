#' Preliminarily cleaning numerical string vectors
#' @description Cleaning illegal characters in string vectors that store numerical values.
#'   The function is useful for cleaning electrical health records in Chinese.
#' @param x A string vector that stores numerical values.
#' @details The function will convert full-width characters to half-width characters, remove
#'   spaces and extra dots, and replace empty strings with `NA`.
#' @returns A string vector that stores cleaner numerical values.
#' @export
#' @examples
#' x <- c("\uFF11\uFF12\uFF13", "11..23", "\uff41\uff42\uff41\uff4e\uff44\uff4f\uff4e")
#' value_initial_cleaning(x)
value_initial_cleaning <- function(x) {
  x <- stringi::stri_trans_general(x, "Fullwidth-Halfwidth")
  x <- str_replace_all(x, c(" " = "", "\\.+" = "\\."))
  x[which(x == "")] <- NA
  x
}
