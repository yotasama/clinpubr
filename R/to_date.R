#' Convert numerical or character date to date.
#' @description Convert numerical (especially Excel date) or character date to date. Can deal with
#'   common formats and allow different formats in one vector.
#' @param x A vector that stores dates in numerical or character types.
#' @param from_excel If TRUE, treat numerical values as Excel dates.
#' @param verbose If TRUE, print the values that cannot be converted.
#' @param try_formats A character vector of date formats to try. Same as `tryFormats` in `as.Date`.
#'
#' @returns A single valid value from the vector. `NA` if all values are invalid.
#' @export
#' @examples
#' to_date(c(43562, "2020-01-01", "2020/01/01", "20200101", "2020.01.01"))
to_date <- function(x, from_excel = TRUE, verbose = TRUE,
                    try_formats = c("%Y-%m-%d", "%Y/%m/%d", "%Y%m%d", "%Y.%m.%d")) {
  if (is.numeric(x)) {
    if (from_excel) {
      y <- as.Date(x, origin = "1899-12-30")
    } else {
      y <- as.Date(x)
    }
  } else if (is.character(x)) {
    y <- .to_date_vectorized(x, from_excel = from_excel, verbose = verbose,
                              try_formats = try_formats)
  } else {
    stop(sprintf("Input must be numeric or character vector, got: %s", class(x)[1]), call. = FALSE)
  }
  y
}

.to_date_vectorized <- function(x, from_excel = TRUE, verbose = TRUE,
                                 try_formats = c("%Y-%m-%d", "%Y/%m/%d", "%Y%m%d", "%Y.%m.%d")) {
  n <- length(x)
  result <- as.Date(rep(NA, n))

  # Identify potential Excel dates (numeric strings < 100000)
  if (from_excel) {
    num_x <- suppressWarnings(as.numeric(x))
    excel_idx <- !is.na(num_x) & num_x < 100000

    if (any(excel_idx, na.rm = TRUE)) {
      result[excel_idx] <- as.Date(num_x[excel_idx], origin = "1899-12-30")
    }
  }

  # Process remaining values with try_formats
  for (fmt in try_formats) {
    still_na <- is.na(result)
    if (!any(still_na, na.rm = TRUE)) break

    parsed <- suppressWarnings(as.Date(x, format = fmt))
    result[is.na(result)] <- parsed[still_na]
  }

  # Report unconverted values if verbose
  if (verbose) {
    failed_idx <- is.na(result) & !is.na(x)
    if (any(failed_idx, na.rm = TRUE)) {
      failed_values <- unique(x[failed_idx])
      for (val in failed_values) {
        message(paste0("cannot process:", val))
      }
    }
  }

  result
}
