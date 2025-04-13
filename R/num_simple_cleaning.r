#' Preliminarily cleaning numerical string vectors
#' @description Cleaning illegal characters in string vectors that store numerical values.
#'   The function is useful for cleaning electrical health records in Chinese.
#' @param x A string vector that stores numerical values.
#' @return A cleaner string vector that stores numerical values.
#' @export
#' @examples
#' num_simple_cleaning(c("１２３＊＿＋", "11..23", "11ａ：　Ａ"))
num_simple_cleaning <- function(x) {
  x <- chartr(
    paste0("\uFF01-\uFF5E\u3000"),
    "!-~ ", x
  )
  x <- str_replace_all(x, c(" " = "", "\\.+" = "\\."))
  x[which(x == "")] <- NA
  x
}
