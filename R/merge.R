#' Merge Data Frame with String Matching
#'
#' This function merges a data frame with a match table based on string containment.
#' It searches for patterns from `match_df[[ori_col]]` in `df[[key_col]]` and
#' adds the corresponding columns from `match_df` to `df`.
#'
#' @param df A data frame to be merged
#' @param match_df A data frame containing matching patterns and values
#' @param key_col Column name in `df` to search in (default: "name")
#' @param ori_col Column name in `match_df` containing patterns to search for (default: "ori")
#' @param new_cols Column name(s) in `match_df` to add to `df` (default: "new")
#'   Can be a single column name or a character vector of column names
#'
#' @return A data frame with all columns from `df` plus matched columns from `match_df`.
#'   Unmatched rows will have NA values in the new columns.
#'
#' @examples
#' # Basic usage
#' df <- data.frame(
#'   name = c("AB", "B,C", "A..", "ACD"),
#'   value = c(1, 2, 3, 4),
#'   stringsAsFactors = FALSE
#' )
#' match_df <- data.frame(
#'   ori = c("A", "B", "C", "ACD", "AB"),
#'   category = c("cat1", "cat2", "cat3", "cat4", "cat1"),
#'   code = c("001", "002", "003", "004", "001"),
#'   stringsAsFactors = FALSE
#' )
#' result <- merge_str_contains(df, match_df, new_cols = c("category", "code"))
#' print(result)
#'
#' @export
merge_str_contains <- function(df, match_df, key_col = "name", ori_col = "ori", new_cols = "new") {
  # Validate inputs
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame")
  }
  if (!is.data.frame(match_df)) {
    stop("`match_df` must be a data frame")
  }
  if (!key_col %in% names(df)) {
    stop(sprintf("Column '%s' not found in `df`", key_col))
  }
  if (!ori_col %in% names(match_df)) {
    stop(sprintf("Column '%s' not found in `match_df`", ori_col))
  }

  # Ensure new_cols is a character vector
  if (length(new_cols) == 1) {
    new_cols <- as.character(new_cols)
  }
  if (!all(new_cols %in% names(match_df))) {
    stop("All `new_cols` must exist in `match_df`")
  }

  # Extract unique key values
  keys_vec <- unique(df[[key_col]])

  # Group match_df by new_cols and extract unique ori patterns for each group
  groups <- match_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(new_cols))) %>%
    dplyr::summarize(
      ori_list = list(unique(na.omit(.data[[ori_col]]))),
      .groups = "drop"
    ) %>%
    dplyr::filter(lengths(ori_list) > 0)

  if (nrow(groups) == 0) {
    return(df)
  }

  # Process each group
  res_list <- vector("list", nrow(groups))
  k <- 0

  for (i in seq_len(nrow(groups))) {
    ori_vec <- groups$ori_list[[i]]
    ori_vec <- ori_vec[!is.na(ori_vec)]

    if (length(ori_vec) == 0) next

    # Escape special regex characters and build pattern
    ori_vec_esc <- Hmisc::escapeRegex(as.character(ori_vec))
    pattern <- paste0("(?:", paste(ori_vec_esc, collapse = "|"), ")")

    # Find matches
    matches <- stringi::stri_detect_regex(keys_vec, pattern)
    matches[is.na(matches)] <- FALSE

    if (any(matches)) {
      matched_keys <- keys_vec[matches]

      if (length(matched_keys) > 0) {
        group_info <- groups[i, new_cols, drop = FALSE] %>%
          as.data.frame(stringsAsFactors = FALSE)

        k <- k + 1
        res_list[[k]] <- cbind(
          data.frame(setNames(list(matched_keys), key_col)),
          group_info[rep(1, length(matched_keys)), , drop = FALSE],
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (k == 0) {
    return(df)
  }

  # Combine results and merge with original df
  match_results <- dplyr::bind_rows(res_list[seq_len(k)])
  rownames(match_results) <- NULL

  # Remove duplicates: keep first match for each key
  match_results <- match_results[!duplicated(match_results[[key_col]]), ]

  result <- merge(df, match_results, by = key_col, all.x = TRUE)
  return(result)
}
