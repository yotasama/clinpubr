#' Check elements that are not numeric
#' @description Finds the elements that cannot be converted to numeric in a character vector.
#'   Useful when setting the strategy to clean numeric values.
#' @param x A string vector that stores numerical values.
#' @param return_idx A logical value.
#'   If `TRUE`, return the index of the elements that are not numeric.
#' @param show_unique A logical value. If `TRUE`, return the unique elements that are not numeric.
#'   Omitted if `return_idx` is `TRUE`.
#' @param max_count An integer. The maximum number of elements to show.
#'   If `NULL` or `0`, show all elements. Omitted if `return_idx` is `TRUE`.
#' @param random_sample A logical value. If `TRUE`, randomly sample the elements to show.
#'   Only works if `max_count` is not `NULL` or `0`.
#' @param fix_len A logical value. If `TRUE`, fill the vector with `NA` to fix the length to
#'   `max_count`.
#' @details The function uses the `as.numeric()` function to try to convert the elements to numeric.
#'   If the conversion fails, the element is considered non-numeric.
#' @returns The (unique) elements that cannot be converted to numeric,
#'   and their indexes if `return_idx` is `TRUE`.
#' @export
#' @examples
#' check_nonnum(c("\uFF11\uFF12\uFF13", "11..23", "3.14", "2.131", "35.2."))
check_nonnum <- function(x, return_idx = FALSE, show_unique = TRUE, max_count = NULL,
                         random_sample = FALSE, fix_len = FALSE) {
  x2 <- suppressWarnings(as.numeric(x))
  idx <- which(!is.na(x) & is.na(x2))
  y <- x[idx]
  if (return_idx) {
    return(list(value = y, idx = idx))
  } else if (show_unique) {
    y <- unique(y)
  }
  if (!is.null(max_count) && max_count > 0) {
    if (length(x) > max_count || fix_len) {
      y <- if (random_sample) sample(y, max_count) else y[1:max_count]
    }
  }
  y
}


#' Show non-numeric elements in a data frame
#' @description Shows the non-numeric elements in a data frame.
#'   Useful when setting the strategy to clean numeric values.
#' @param df A data frame.
#' @param max_count An integer. The maximum number of elements to show for each column.
#'   If `NULL` or `0`, show all elements, not recommended due to huge memory waste.
#' @param random_sample A logical value. If `TRUE`, randomly sample the elements to show.
#' @param long_df A logical value. If `TRUE`, the input `df` is provided in a long format.
#' @param subject_col A character string. The name of the column that contains the subject
#'   identifier. Used when `long_df` is `TRUE`.
#'   If `NULL`, the subject column is assumed to be the first column.
#' @param value_col A character string. The name of the column that contains the values.
#'   Used when `long_df` is `TRUE`.
#'   If `NULL`, the value column is assumed to be the second column.
#' @returns A data frame of the non-numeric elements.
#' @export
#' @examples
#' df <- data.frame(
#'   x = c("1", "2", "3..3", "4", "6a"),
#'   y = c("1", "ss", "aa.a", "4", "xx"),
#'   z = c("1", "2", "3", "4", "6")
#' )
#' df_view_nonnum(df)
df_view_nonnum <- function(df, max_count = 20, random_sample = FALSE, long_df = FALSE,
                           subject_col = NULL, value_col = NULL) {
  if (ncol(df) == 0) {
    return(data.frame())
  }
  if (long_df && ncol(df) != 2 && (is.null(subject_col) || is.null(value_col))) {
    stop("`subject_col` and `value_col` must be specified for long `data.frame` with more than 2 columns.")
  }
  if (long_df) {
    if (inherits(df, "data.frame")) {
      cols <- colnames(df)
    } else if (inherits(df, "dtplyr_step")) {
      cols <- df$vars
    }
    if (is.null(subject_col)) subject_col <- cols[1]
    if (is.null(value_col)) value_col <- cols[2]
    df_long <- df %>% dplyr::select(!!rlang::sym(subject_col), !!rlang::sym(value_col))
  } else {
    subject_col <- "subject"
    value_col <- "value"
    df_long <- df %>%
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = subject_col,
        values_to = value_col
      )
  }
  if (is.null(max_count) || max_count == 0) {
    max_count <- df_long %>%
      dplyr::count(!!rlang::sym(subject_col)) %>%
      dplyr::pull(n) %>%
      max()
  }

  tmp_fun <- function(a, b) {
    x <- check_nonnum(a[[value_col]],
      max_count = max_count,
      random_sample = random_sample,
      fix_len = TRUE
    )
    dplyr::tibble(row = seq_along(x), value = x)
  }

  res <- df_long %>%
    dplyr::group_by(!!rlang::sym(subject_col)) %>%
    dplyr::group_modify(tmp_fun) %>%
    as_tibble() %>%
    tidyr::pivot_wider(
      names_from = !!rlang::sym(subject_col),
      values_from = value,
      values_fill = NA
    ) %>%
    dplyr::select(-row) %>%
    dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.x))) %>%
    dplyr::select(dplyr::where(~ any(!is.na(.x))))

  res
}
