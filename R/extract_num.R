#' Extract numbers from string.
#' @description Extract numerical values from strings. Can be used to filter out the unwanted information
#'   coming along with the numbers.
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
#' extract_num(x,
#'   res_type = "first", multimatch2na = TRUE, zero_regexp = "NEG|NS",
#'   max_regexp = "FULL"
#' )
#' extract_num(x, res_type = "range", allow_neg = FALSE, zero_regexp = "NEG|NS", max_regexp = "FULL")
extract_num <- function(x, res_type = c("first", "range"), multimatch2na = FALSE, leq_1 = FALSE,
                        allow_neg = TRUE, zero_regexp = NULL, max_regexp = NULL, max_quantile = 0.95) {
  if (is.numeric(x)) {
    return(x)
  }

  # Fast path: try direct conversion for pure numeric strings
  try_res <- suppressWarnings(as.numeric(x))
  if (all(is.na(try_res) == is.na(x))) {
    return(try_res)
  }

  res_type <- match.arg(res_type)

  # Precompute flags
  flag_zero <- if (!is.null(zero_regexp)) grepl(zero_regexp, x) else logical(length(x))
  flag_max <- if (!is.null(max_regexp)) grepl(max_regexp, x) else logical(length(x))

  if (allow_neg) {
    if (res_type == "range") warning("`allow_neg` is `TRUE`! Make sure you do not use '-' to connect numbers!")
    my_expr <- "-?[0-9]+\\.?[0-9]*|-?\\.[0-9]+"
  } else {
    my_expr <- "[0-9]+\\.?[0-9]*|\\.[0-9]+"
  }

  if (res_type == "first") {
    # For "first" mode, only extract the first match - much faster
    first_matches <- stringi::stri_extract_first_regex(x, my_expr)
    res <- suppressWarnings(as.numeric(first_matches))

    if (multimatch2na) {
      # Need to check if there are multiple matches
      all_matches <- stringi::stri_extract_all_regex(x, my_expr)
      match_lens <- lengths(all_matches)
      res[match_lens != 1] <- NA_real_
    }
    if (leq_1) {
      res[res > 1] <- NA_real_
    }
  } else if (res_type == "range") {
    # For "range" mode, need all matches to calculate mean of range
    match_res <- stringi::stri_extract_all_regex(x, my_expr)
    match_lens <- lengths(match_res)

    n <- length(x)
    res <- rep(NA_real_, n)

    # Process single match
    len1 <- match_lens == 1
    if (any(len1)) {
      res[len1] <- as.numeric(vapply(match_res[len1], `[`, character(1), 1))
    }

    # Process double match (range)
    len2 <- match_lens == 2
    if (any(len2)) {
      firsts <- as.numeric(vapply(match_res[len2], `[`, character(1), 1))
      seconds <- as.numeric(vapply(match_res[len2], `[`, character(1), 2))
      res[len2] <- (firsts + seconds) / 2
    }
  }

  # Apply special value flags
  if (!is.null(max_regexp) && any(flag_max)) {
    res[flag_max] <- quantile(res, max_quantile, na.rm = TRUE, names = FALSE)
  }
  if (!is.null(zero_regexp) && any(flag_zero)) {
    res[flag_zero] <- 0
  }

  res
}
