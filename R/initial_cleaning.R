#' Auto-detect and repair encoding issues in character vectors
#' @description Detects non-UTF-8 encoded characters and attempts to repair them.
#'   Uses base::validUTF8() for fast detection and stringi for conversion.
#'   This is useful for handling data imported from various sources with mixed encodings.
#' @param x A character vector.
#' @param from_encoding Character, the source encoding to convert from.
#'   If "auto" (default), attempts to detect the most likely encoding.
#'   Common values: "GBK", "GB2312", "Latin-1", "UTF-8".
#' @returns A character vector with repaired encoding.
#' @note This function will warn when encoding repairs are made.
#' @keywords internal
#' @examples
#' # UTF-8 input (no repair needed)
#' auto_encoding_repair(c("hello", "world"))
#'
#' # Full-width characters (UTF-8, no repair needed)
#' auto_encoding_repair(c("\uFF11\uFF12\uFF13", "abc"))
#'
#' # Simulate GBK-encoded Chinese characters that need repair
#' # (using iconv to create non-UTF-8 bytes for demonstration)
#' if (l10n_info()$"UTF-8") {
#'   # Create GBK-encoded bytes from UTF-8 Chinese characters
#'   gbk_bytes <- iconv("\u4f60\u597d", from = "UTF-8", to = "GBK")
#'   auto_encoding_repair(gbk_bytes)
#' }
#'
#' # Simulate Latin-1 encoded characters
#' if (l10n_info()$"UTF-8") {
#'   latin1_bytes <- iconv("caf\u00e9", from = "UTF-8", to = "Latin-1")
#'   auto_encoding_repair(latin1_bytes)
#' }
#'
#' # Mixed encoding vector (UTF-8 and GBK)
#' if (l10n_info()$"UTF-8") {
#'   mixed <- c("hello",
#'              iconv("\u4e2d\u6587", from = "UTF-8", to = "GBK"),
#'              "world")
#'   auto_encoding_repair(mixed)
#' }
auto_encoding_repair <- function(x, from_encoding = "auto") {
  # Fast path: non-character input
  if (!is.character(x)) {
    return(x)
  }

  n <- length(x)

  # Fast path: empty vector
  if (n == 0L) {
    return(x)
  }

  # Fast path: check if all elements are valid UTF-8
  # validUTF8 is implemented in C and very fast
  utf8_valid <- validUTF8(x)
  if (all(utf8_valid)) {
    return(x)
  }

  # Find indices of non-UTF-8 elements
  # useNames = FALSE avoids overhead of creating names attribute
  non_utf8_idx <- which(!utf8_valid, useNames = FALSE)
  n_non_utf8 <- length(non_utf8_idx)

  # Extract non-UTF-8 elements for processing
  x_non_utf8 <- x[non_utf8_idx]

  # Determine encodings to try
  encodings_to_try <- if (from_encoding == "auto") {
    c("GBK", "GB2312", "latin1", "UTF-8")
  } else {
    from_encoding
  }

  # Try each encoding
  converted <- NULL
  success <- FALSE

  for (enc in encodings_to_try) {
    converted <- tryCatch(
      stringi::stri_encode(x_non_utf8, from = enc, to = "UTF-8"),
      error = function(e) NULL
    )

    if (is.null(converted)) {
      next
    }

    # Fast validation checks
    if (any(is.na(converted)) || 
      !all(validUTF8(converted)) ||
      any(stringi::stri_detect_fixed(converted, "\ufffd")) ||
      any(stringi::stri_detect_fixed(converted, "\u001a"))) {
      next
    }
    # All checks passed
    success <- TRUE
    break
  }

  # Prepare result
  if (success) {
    # Create result vector
    x_repaired <- x
    x_repaired[non_utf8_idx] <- converted

    warning(sprintf(
      "Detected and repaired %d elements with non-UTF-8 encoding.",
      n_non_utf8
    ))
    return(x_repaired)
  }

  # Repair failed: set non-UTF-8 elements to NA
  x_repaired <- x
  x_repaired[non_utf8_idx] <- NA_character_

  warning(sprintf(
    "Detected %d elements with non-UTF-8 encoding. All failed to repair and were set to NA.",
    n_non_utf8
  ))

  x_repaired
}


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
#' @param remove_inequal A logical value. If `TRUE`, remove comparison symbols
#'   such as `<`, `>` from the string
#' @param fix_encoding Logical. If `TRUE`, automatically detect and repair non-UTF-8
#'   encoding issues before cleaning. Default is `TRUE`.
#' @returns A string vector with less illegal characters.
#' @note When `fix_encoding = TRUE`, a warning will be issued if encoding repairs are made.
#' @export
#' @examples
#' x <- c("\uFF11\uFF12\uFF13", "11..23", "\uff41\uff42\uff41\uff4e\uff44\uff4f\uff4e",
#'        "hello world ")
#' value_initial_cleaning(x)
#' char_initial_cleaning(x)
value_initial_cleaning <- function(x, remove_inequal = FALSE, fix_encoding = TRUE) {
  if (fix_encoding) {
    x <- auto_encoding_repair(x)
  }
  x <- stringi::stri_trans_general(x, "Fullwidth-Halfwidth")
  x <- str_replace_all(x, c("\\.+" = "\\."))
  if (remove_inequal) {
    x <- str_remove_all(x, "[ <>\u2264\u2265&lt;&gt;]")
  } else {
    x <- str_remove_all(x, " ")
  }
  x[which(x == "")] <- NA
  x
}


#' @rdname value_initial_cleaning
#' @param fix_encoding Logical. If `TRUE`, automatically detect and repair non-UTF-8
#'   encoding issues before cleaning. Default is `TRUE`.
#' @export
char_initial_cleaning <- function(x, fix_encoding = TRUE) {
  if (fix_encoding) {
    x <- auto_encoding_repair(x)
  }
  x <- stringi::stri_trans_general(x, "Fullwidth-Halfwidth")
  x <- str_squish(x)
  x[which(x == "")] <- NA
  x
}
