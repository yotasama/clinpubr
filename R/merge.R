#' Merge Data Frame by String Key Matching
#'
#' @description This function merges two data frames based on string key matching.
#' It searches for keys from `key_df[[key_col]]` in `data[[search_col]]`
#' and adds corresponding columns from `key_df` to `data`.
#'
#' @param data The primary data frame to be enhanced with additional columns
#' @param key_df A data frame containing string keys and their corresponding values
#' @param search_col Column name in `data` to search for keys (default: "name")
#' @param key_col Column name in `key_df` containing keys to match (default: "key")
#' @param value_cols Column name(s) in `key_df` to add to `data` (default: "value")
#'   Can be a single column name or a character vector of column names
#'
#' @return A data frame with all columns from `data` plus matched columns from `key_df`.
#'   Unmatched rows will have NA values in the added columns.
#'
#' @examples
#' # Basic usage
#' main_data <- data.frame(
#'   name = c("AB", "B,C", "A..", "ACD"),
#'   value = c(1, 2, 3, 4),
#'   stringsAsFactors = FALSE
#' )
#' key_lookup <- data.frame(
#'   key = c("A", "B", "C", "ACD", "AB"),
#'   category = c("cat1", "cat2", "cat3", "cat4", "cat1"),
#'   code = c("001", "002", "003", "004", "001"),
#'   stringsAsFactors = FALSE
#' )
#' result <- merge_by_substring(main_data, key_lookup,
#'   search_col = "name",
#'   key_col = "key", value_cols = c("category", "code")
#' )
#' print(result)
#'
#' @export
merge_by_substring <- function(data, key_df, search_col, key_col, value_cols) {
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame")
  }
  if (!is.data.frame(key_df)) {
    stop("`key_df` must be a data frame")
  }
  if (!search_col %in% names(data)) {
    stop(sprintf("Column '%s' not found in `data`", search_col))
  }
  if (!key_col %in% names(key_df)) {
    stop(sprintf("Column '%s' not found in `key_df`", key_col))
  }

  # Ensure value_cols is a character vector
  if (length(value_cols) == 1) {
    value_cols <- as.character(value_cols)
  }
  if (!all(value_cols %in% names(key_df))) {
    stop("All `value_cols` must exist in `key_df`")
  }

  # Extract unique key values
  search_values <- unique(data[[search_col]])

  # Group key_df by value_cols and extract unique keys for each group
  groups <- key_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(value_cols))) %>%
    dplyr::summarize(
      key_list = list(unique(na.omit(.data[[key_col]]))),
      .groups = "drop"
    ) %>%
    dplyr::filter(lengths(key_list) > 0)

  if (nrow(groups) == 0) {
    return(data)
  }

  # Process each group
  match_list <- vector("list", nrow(groups))
  k <- 0

  for (i in seq_len(nrow(groups))) {
    keys <- groups$key_list[[i]]
    keys <- keys[!is.na(keys)]

    # Escape special regex characters and build pattern
    keys_escaped <- Hmisc::escapeRegex(as.character(keys))
    regex_pattern <- paste0("(?:", paste(keys_escaped, collapse = "|"), ")")

    # Find matches
    matches <- stringi::stri_detect_regex(search_values, regex_pattern)
    matches[is.na(matches)] <- FALSE

    if (any(matches)) {
      matched_keys <- search_values[matches]

      if (length(matched_keys) > 0) {
        group_info <- groups[i, value_cols, drop = FALSE] %>%
          as.data.frame(stringsAsFactors = FALSE)

        k <- k + 1
        match_list[[k]] <- cbind(
          data.frame(setNames(list(matched_keys), search_col)),
          group_info[rep(1, length(matched_keys)), , drop = FALSE],
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (k == 0) {
    return(data)
  }

  # Combine results and merge with original data
  match_results <- dplyr::bind_rows(match_list[seq_len(k)])
  rownames(match_results) <- NULL

  result <- merge(data, match_results, by = search_col, all.x = TRUE)
  return(result)
}
