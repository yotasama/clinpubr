#' Convert numerical or character date to date.
#' @description Convert numerical (especially Excel date) or character date to date. Can deal with
#'   common formats and allow different formats in one vector.
#' @param x A vector that stores dates in numerical or character types.
#' @param from_excel If TRUE, treat numerical values as Excel dates.
#' @param print_failure If TRUE, print the values that cannot be converted.
#' @param try_formats A character vector of date formats to try. Same as `tryFormats` in `as.Date`.
#'
#' @returns A single valid value from the vector. `NA` if all values are invalid.
#' @export
#' @examples
#' to_date(c(43562, "2020-01-01", "2020/01/01", "20200101", "2020.01.01"))
to_date <- function(x, from_excel = T, print_failure = T,
                    try_formats = c("%Y-%m-%d", "%Y/%m/%d", "%Y%m%d", "%Y.%m.%d")) {
  if (is.numeric(x)) {
    if (from_excel) {
      y <- as.Date(x, origin = "1899-12-30")
    } else {
      y <- as.Date(x)
    }
  } else {
    y <- as.Date(sapply(x, .to_date,
      from_excel = from_excel, print_failure = print_failure,
      try_formats = try_formats, USE.NAMES = FALSE
    ))
  }
  y
}

.to_date <- function(x, from_excel = T, print_failure = T,
                     try_formats = c("%Y-%m-%d", "%Y/%m/%d", "%Y%m%d", "%Y.%m.%d")) {
  if (suppressWarnings((!is.na(as.numeric(x))) && (as.numeric(x) < 100000) && from_excel)) {
    as.Date(as.numeric(x), origin = "1899-12-30")
  } else {
    tryCatch(
      {
        as.Date(as.character(x), tryFormats = try_formats)
      },
      error = function(e) {
        if (print_failure) {
          print(paste0("cannot process:", as.character(x)))
        }
        NA
      }
    )
  }
}