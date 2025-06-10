#' Check elements that are not numeric
#' @description Finds the elements that cannot be converted to numeric in a character vector.
#'   Useful when setting the strategy to clean numeric values.
#' @param x A string vector that stores numerical values.
#' @param return_idx A logical value. If `TRUE`, return the index of the elements that are not numeric.
#' @param show_unique A logical value. If `TRUE`, return the unique elements that are not numeric.
#'   Omitted if `return_idx` is `TRUE`.
#' @details The function uses the `as.numeric()` function to try to convert the elements to numeric.
#'   If the conversion fails, the element is considered non-numeric.
#' @returns The (unique) elements that cannot be converted to numeric, and their indexes if `return_idx` is `TRUE`.
#' @export
#' @examples
#' check_nonnum(c("\uFF11\uFF12\uFF13", "11..23", "3.14", "2.131", "35.2."))
check_nonnum <- function(x, return_idx = FALSE, show_unique = TRUE) {
  x2 <- suppressWarnings(as.numeric(x))
  idx <- which(!is.na(x) & is.na(x2))
  y <- x[idx]
  if (return_idx) {
    list(value = y, idx = idx)
  } else if (show_unique) {
    unique(y)
  } else {
    y
  }
}


#' Show non-numeric elements in a data frame
#' @description Shows the non-numeric elements in a data frame.
#'   Useful when setting the strategy to clean numeric values.
#' @param df A data frame.
#' @param max_count An integer. The maximum number of elements to show for each column.
#'   If `NULL` or `0`, show all elements.
#' @param random_sample A logical value. If `TRUE`, randomly sample the elements to show.
#' @returns A data frame of the non-numeric elements.
#' @export
#' @examples
#' df <- data.frame(
#'   x = c("1", "2", "3..3", "4", "6a"),
#'   y = c("1", "ss", "aa.a", "4", "xx"),
#'   z = c("1", "2", "3", "4", "6")
#' )
df_view_nonnum <- function(df, max_count = 20, random_sample = FALSE) {
  if (ncol(df) == 0) return(data.frame())  # Handle empty data frame
  if (is.null(max_count) || max_count == 0) {
    max_count <- nrow(df)
  }
  res <- data.frame(matrix(NA, nrow = max_count, ncol = ncol(df)))
  colnames(res) <- colnames(df)
  for (i in 1:ncol(df)) {
    x <- check_nonnum(df[, i])
    if (length(x) > max_count) {
      if (random_sample) {
        x <- sample(x, max_count)
      } else {
        x <- x[1:max_count]
      }
    }
    res[seq_along(x), i] <- x
  }
  # Remove rows with all NAs
  if (nrow(res) > 0) {
    res <- res[rowSums(!is.na(res)) > 0, , drop = FALSE]
  } else {
    return(data.frame())
  }

  # Remove columns with all NAs (handle empty case)
  if (ncol(res) > 0) {
    res <- res[, colSums(!is.na(res)) > 0, drop = FALSE]
  } else {
    return(data.frame())
  }
  res
}
