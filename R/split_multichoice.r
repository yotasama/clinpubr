#' @include utils.R
NULL
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
#' df <- data.frame(q1 = c("a b", "c d a", "b a"), q2 = c("a b", "a c", "d"))
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
      df[, paste0(col, link, opt)] <- sapply(tmp_split, function(x) opt %in% x)
    }
  }
  if (remove_cols) {
    df <- df[, !(colnames(df) %in% quest_cols)]
  }

  return(df)
}
