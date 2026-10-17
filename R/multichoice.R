#' Split multi-choice data into columns
#' @description Split multi-choice data into columns, each new column consists of
#'   booleans whether a choice is presented.
#' @param df A data frame.
#' @param quest_cols A vector of column names that contain multi-choice data.
#' @param split A string to split the data. Default is `""`.
#' @param remove_space If `TRUE`, remove space in the data.
#' @param link A string to link the column name and the option. Default is `"_"`.
#' @param remove_cols If `TRUE`, remove the original columns.
#'
#' @returns A data frame with additional columns.
#' @export
#' @examples
#' df <- data.frame(q1 = c("ab", "c da", "b a", NA), q2 = c("a b", "a c", "d", "ab"))
#' split_multichoice(df, quest_cols = c("q1", "q2"))
split_multichoice <- function(df, quest_cols, split = "", remove_space = TRUE,
                              link = "_", remove_cols = TRUE) {
  # Precompute the (possibly cleaned) column contents once per question column.
  # In the original implementation strsplit / sapply were invoked per option
  # inside the option loop, which scaled quadratically: for a column with many
  # distinct options the whole column was re-split and re-tested for every
  # option.  A single split + a vectorized membership table avoids this.
  for (col in quest_cols) {
    values <- df[, col]
    if (remove_space) {
      values <- str_remove_all(values, " ")
    }

    if (nchar(split) <= 1) {
      tmp_split <- strsplit(values, split, fixed = TRUE)
    } else {
      tmp_split <- strsplit(values, split)
    }

    # A row with an NA input yields NA options (kept for backward compatibility:
    # option columns are NA for missing input rows).
    row_has_na <- is.na(values)
    n <- length(tmp_split)
    options <- na.omit(unique(unlist(tmp_split, use.names = FALSE)))

    # Vectorized membership: build one TRUE/FALSE matrix for all options at
    # once instead of looping over options with lapply/sapply.
    membership <- matrix(FALSE,
      nrow = n, ncol = length(options),
      dimnames = list(NULL, options)
    )
    if (n > 0 && length(options) > 0) {
      # Which list elements are non-empty, and the flattened (row index, option
      # index) pairs for the selected options.  data.table::rbindlist on list
      # columns is much faster than do.call(rbind, ...) on a data.frame.
      lens <- lengths(tmp_split)
      row_idx <- rep.int(seq_len(n), lens)
      opt_idx <- match(unlist(tmp_split, use.names = FALSE), options)
      # In case of duplicate options within one row, the same cell may be set
      # repeatedly (idempotent); matrix subscripting keeps it simple.
      membership[cbind(row_idx, opt_idx)] <- TRUE
    }

    col_names <- paste0(col, link, options)
    for (k in seq_along(options)) {
      new_col <- membership[, k]
      new_col[row_has_na] <- NA
      df[, col_names[k]] <- new_col
    }
  }
  if (remove_cols) {
    df <- df[, !(colnames(df) %in% quest_cols)]
  }

  return(df)
}

#' Combine multi-choice columns into one
#' @description Combine multi-choice columns into one, each column consists of
#'   booleans whether a choice is presented.
#' @param df A data frame.
#' @param quest_cols A named list where each element is a character vector of column
#'   names to combine, or a single character vector.
#' @param sep A string to separate the data. Default is `","`.
#' @param remove_cols If `TRUE`, remove the original columns.
#' @param remove_prefix If `TRUE`, automatically remove common prefix from column names when combining.
#'
#' @returns A data frame with additional columns.
#' @export
#' @examples
#' # Single group (backward compatibility)
#' df <- data.frame(q1 = c(TRUE, FALSE, TRUE), q2 = c(FALSE, TRUE, TRUE))
#' combine_multichoice(df, quest_cols = c("q1", "q2"))
#'
#' # Multiple groups with named list
#' df <- data.frame(
#'   a1 = c(TRUE, FALSE, TRUE), a2 = c(FALSE, TRUE, TRUE),
#'   b1 = c(TRUE, TRUE, FALSE), b2 = c(FALSE, FALSE, TRUE)
#' )
#' combine_multichoice(df, quest_cols = list(groupA = c("a1", "a2"), groupB = c("b1", "b2")))
combine_multichoice <- function(df, quest_cols, sep = ",", remove_cols = TRUE, remove_prefix = TRUE) {
  if (!is.list(quest_cols)) {
    if (is.null(names(quest_cols))) {
      quest_cols <- list(combined_multichoice = quest_cols)
    } else {
      quest_cols <- as.list(quest_cols)
    }
  }

  if (is.null(names(quest_cols)) || any(names(quest_cols) == "")) {
    stop("All elements of quest_cols must be named")
  }

  all_cols <- unlist(quest_cols)

  if (!all(all_cols %in% colnames(df))) {
    missing_cols <- setdiff(all_cols, colnames(df))
    stop("The following columns are not present in the data frame: ", paste(missing_cols, collapse = ", "))
  }

  col_classes <- vapply(df[, all_cols, drop = FALSE], function(x) class(x)[1], character(1))
  non_logical <- names(col_classes)[col_classes != "logical"]
  if (length(non_logical) > 0) {
    stop("The following columns are not logical vectors: ", paste(non_logical, collapse = ", "))
  }

  for (group_name in names(quest_cols)) {
    group_cols <- quest_cols[[group_name]]

    # Handle auto-naming by removing common prefix
    if (remove_prefix && length(group_cols) > 1) {
      prefix <- common_prefix(group_cols)
      trimmed_names <- if (prefix == "") group_cols else substr(group_cols, nchar(prefix) + 1, nchar(group_cols))
      if (all(trimmed_names == "")) {
        name_map <- setNames(group_cols, group_cols)
      } else {
        name_map <- setNames(trimmed_names, group_cols)
      }
    } else {
      name_map <- setNames(group_cols, group_cols)
    }

    # Vectorized per-column accumulation
    n <- nrow(df)
    col_names_vec <- unname(name_map[group_cols])
    result <- rep("", n)
    for (j in seq_along(group_cols)) {
      col_vals <- df[[group_cols[j]]]
      col_vals[is.na(col_vals)] <- FALSE
      if (any(col_vals)) {
        name_j <- col_names_vec[j]
        if (sep == "") {
          result[col_vals] <- paste0(result[col_vals], name_j)
        } else {
          needs_sep <- nzchar(result[col_vals])
          result[col_vals] <- paste0(
            result[col_vals],
            ifelse(needs_sep, sep, ""),
            name_j
          )
        }
      }
    }
    df[[group_name]] <- result
  }

  if (remove_cols) {
    df <- df[, !colnames(df) %in% all_cols, drop = FALSE]
  }

  return(df)
}
