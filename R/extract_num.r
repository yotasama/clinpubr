#' Extract numbers from string.
#' @description Exract numberical values from strings. Can be used to filter out the unwanted information
#'   comming along with the numbers.
#' @param x A character vector.
#' @param res_type The type of the result. Can be `"first"` or `"range"`. If `"first"`, the first number in the string
#'   is extracted. If `"range"`, the mean of the range in the string is extracted.
#' @param multimatch2na If `TRUE`, multiple matches will be converted to `NA`. Only works when `res_type` is `"first"`.
#' @param leq_1 If `TRUE`, numbers greater than 1 will be converted to `NA`. Only works when `res_type` is `"first"`.
#' @param allow_neg If `TRUE`, negative numbers are allowed. Otherwise, only positive numbers are allowed.
#' @param zero_regexp A regular expression to match the string that indicates zero.
#' @param max_regexp A regular expression to match the string that indicates the maximum value.
#' @param max_quantile The quantile of values to set the maximum value to.
#' @details The function uses regular expressions to extract numbers from strings. The regular expression used is
#'   `"-?[0-9]+\\.?[0-9]*|-?\\.[0-9]+"`, which matches any number that may have a decimal point and may have a
#'   negative sign.
#' @returns A numeric vector.
#' @export
#' @examples
#' x <- c("1.2(XXX)", "5-8POS", "NS", "FULL", "5.5", "4.2")
#' extract_num(x)
#' extract_num(x, res_type = "first", multimatch2na = TRUE, zero_regexp = "NEG|NS", max_regexp = "FULL")
#' extract_num(x, res_type = "range", allow_neg = FALSE, zero_regexp = "NEG|NS", max_regexp = "FULL")
extract_num <- function(x, res_type = c("first", "range"), multimatch2na = FALSE, leq_1 = FALSE,
                        allow_neg = TRUE, zero_regexp = NULL, max_regexp = NULL, max_quantile = 0.95) {
  res_type <- match.arg(res_type)
  if (!is.null(zero_regexp)) {
    flag_zero <- grepl(zero_regexp, x)
  }
  if (!is.null(max_regexp)) {
    flag_max <- grepl(max_regexp, x)
  }
  if (allow_neg) {
    my_expr <- "-?[0-9]+\\.?[0-9]*|-?\\.[0-9]+"
  } else {
    my_expr <- "[0-9]+\\.?[0-9]*|\\.[0-9]+"
  }
  match_res <- regmatches(x, gregexpr(my_expr, x))
  if (res_type == "first") {
    res <- as.numeric(sapply(match_res, `[`, 1))
    if (multimatch2na) {
      res[sapply(match_res, length) != 1] <- NA
    }
    if (leq_1) {
      res[res > 1] <- NA
    }
  } else if (res_type == "range") {
    res <- ifelse(
      sapply(match_res, length) == 1,
      as.numeric(sapply(match_res, `[`, 1)),
      ifelse(
        sapply(match_res, length) == 2,
        (as.numeric(sapply(match_res, `[`, 1)) + as.numeric(sapply(match_res, `[`, 2))) / 2,
        NA
      )
    )
  }
  if (!is.null(max_regexp)) {
    res[flag_max] <- quantile(res, max_quantile, na.rm = TRUE, names = FALSE)
  }
  if (!is.null(zero_regexp)) {
    res[flag_zero] <- 0
  }
  res
}
