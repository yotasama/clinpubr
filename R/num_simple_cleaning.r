#' Preliminarily cleaning numerical string vectors
#' @description Cleaning illegal characters in string vectors that store numerical values.
#'   The function is useful for cleaning electrical health records in Chinese.
#' @param x A string vector that stores numerical values.
#' @return A cleaner string vector that stores numerical values.
#' @export
#' @examples
#' num_simple_cleaning(c("１２３", "11..23", "11ａ："))
num_simple_cleaning <- function(x) {
  x <- chartr(
    # "０-９Ａ-Ｚａ-ｚ．！＠＃＄％＾＆＊：＝（）＿＋",
    paste0("\uFF10-\uFF19\uFF21-\uFF3A\uFF41-\uFF5A\uFF0E\uFF01\uFF20\uFF03",
           "\uFF04\uFF05\uFF3E\uFF06\uFF0A\uFF1A\uFF1D\uFF08\uFF09\uFF3F\uFF0B"),
    "0-9A-Za-z.!@#$%^&*:=()_+", x
  )
  x <- str_replace_all(x, c(" " = "", "\\.+" = "\\."))
  x[which(x == "")] <- NA
  x
}
