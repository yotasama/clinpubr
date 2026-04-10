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
auto_encoding_repair <- function(x, from_encoding = "auto") {
  if (!is.character(x)) {
    return(x)
  }

  # Check for non-UTF-8 elements using base::validUTF8() for performance
  non_utf8_idx <- which(!validUTF8(x))

  if (length(non_utf8_idx) == 0) {
    return(x)
  }

  # Try to repair encoding
  x_repaired <- x
  repaired_count <- 0

  if (from_encoding == "auto") {
    # Try common Chinese encodings first, then fall back to Latin-1
    encodings_to_try <- c("GBK", "GB2312", "Latin-1", "UTF-8")
  } else {
    encodings_to_try <- from_encoding
  }

  for (enc in encodings_to_try) {
    tryCatch({
      converted <- stringi::stri_encode(x[non_utf8_idx], from = enc, to = "UTF-8")
      # Verify conversion worked (no NA introduced unless original was NA)
      if (sum(is.na(converted) & !is.na(x[non_utf8_idx])) == 0) {
        x_repaired[non_utf8_idx] <- converted
        repaired_count <- length(non_utf8_idx)
        break
      }
    }, error = function(e) NULL)
  }

  # If repair failed, set non-UTF-8 elements to NA
  failed_count <- length(non_utf8_idx) - repaired_count
  if (failed_count > 0) {
    x_repaired[non_utf8_idx] <- NA
    warning(sprintf(
      "Detected %d elements with non-UTF-8 encoding. %d elements failed to repair and were set to NA.",
      length(non_utf8_idx),
      failed_count
    ))
  } else {
    warning(sprintf(
      "Detected and repaired %d elements with non-UTF-8 encoding.",
      repaired_count
    ))
  }

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