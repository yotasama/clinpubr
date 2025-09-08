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
  for (col in quest_cols) {
    if (remove_space) {
      df[, col] <- str_remove_all(df[, col], " ")
    }

    if (str_length(split) <= 1) {
      tmp_split <- strsplit(df[, col], split, fixed = TRUE)
    } else {
      tmp_split <- strsplit(df[, col], split)
    }

    unique_options <- na.omit(unique(unlist(tmp_split)))

    for (opt in unique_options) {
      df[, paste0(col, link, opt)] <- ifelse(
        is.na(tmp_split), NA,
        sapply(tmp_split, function(x) opt %in% x)
      )
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

  col_classes <- sapply(df[, all_cols], class)
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
      # Use original names if all trimmed names are empty
      if (all(trimmed_names == "")) {
        name_map <- setNames(group_cols, group_cols)
      } else {
        name_map <- setNames(trimmed_names, group_cols)
      }
    } else {
      name_map <- setNames(group_cols, group_cols)
    }

    df[[group_name]] <- apply(df[, group_cols, drop = FALSE], 1, function(row) {
      row[is.na(row)] <- FALSE
      selected <- names(row)[row]
      paste(name_map[selected], collapse = sep)
    })
  }

  if (remove_cols) {
    df <- df[, !colnames(df) %in% all_cols, drop = FALSE]
  }

  return(df)
}
