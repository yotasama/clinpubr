#' Get an overview of different subjects in data.
#' @description Get a table of subject details for the clinical data.
#'   This table could be labeled and used for subject name standardization.
#' @param df A data frame of medical records that contains test subject, value, and unit cols.
#' @param subject_col The name of the subject column.
#' @param info_cols The names of the columns to get detailed information.
#' @param value_col The name of the column that contains values. This column must be numerical.
#' @param info_n_samples The number of samples to show in the detailed information columns.
#' @param info_collapse The separator to use for collapsing the detailed information.
#' @param info_unique A logical value indicating whether to show unique values only.
#' @param save_table A logical value indicating whether to save the table to a csv file.
#' @param filename The name of the csv file to be saved.
#'
#' @returns A data frame of subject details.
#' @export
#' @examples
#' df <- data.frame(subject = sample(c("a", "b"), 1000, replace = TRUE), value = runif(1000))
#' df$unit <- NA
#' df$unit[df$subject == "a"] <- sample(c("mg/L", "g/l", "g/L"),
#'   sum(df$subject == "a"),
#'   replace = TRUE
#' )
#' df$value[df$subject == "a" & df$unit == "mg/L"] <-
#'   df$value[df$subject == "a" & df$unit == "mg/L"] * 1000
#' df$unit[df$subject == "b"] <- sample(c(NA, "g", "mg"), sum(df$subject == "b"), replace = TRUE)
#' df$value[df$subject == "b" & df$unit %in% "mg"] <-
#'   df$value[df$subject == "b" & df$unit %in% "mg"] * 1000
#' df$value[df$subject == "b" & is.na(df$unit)] <- df$value[df$subject == "b" & is.na(df$unit)] *
#'   sample(c(1, 1000), size = sum(df$subject == "b" & is.na(df$unit)), replace = TRUE)
#' subject_view(
#'   df = df, subject_col = "subject", info_cols = c("value", "unit"), value_col = "value",
#'   save_table = FALSE
#' )
subject_view <- function(df, subject_col, info_cols, value_col = NULL, info_n_samples = 10, info_collapse = "\n",
                         info_unique = FALSE, save_table = FALSE, filename = NULL) {
  if (!is.null(value_col) && is.numeric(df[[value_col]])) {
    res <- df %>%
      group_by(!!as.symbol(subject_col)) %>%
      summarise(
        target_subject = NA,
        count = n(),
        nvalid = sum(!is.na(!!as.symbol(value_col))),
        mean = mean(!!as.symbol(value_col), na.rm = TRUE),
        sd = sd(!!as.symbol(value_col), na.rm = TRUE),
        median = median(!!as.symbol(value_col), na.rm = TRUE),
        across(all_of(info_cols), ~ get_samples(.x,
          n_samples = info_n_samples,
          collapse = info_collapse, unique_only = info_unique
        ))
      )
  } else {
    res <- df %>%
      group_by(!!as.symbol(subject_col)) %>%
      summarise(
        target_subject = NA,
        count = n(),
        across(all_of(info_cols), ~ get_samples(.x,
          n_samples = info_n_samples,
          collapse = info_collapse, unique_only = info_unique
        ))
      )
  }

  res <- as.data.frame(res)
  if (save_table) {
    if (is.null(filename)) {
      filename <- "subject_view.csv"
    }
    write.csv(res, file = filename, na = "")
  }
  return(res)
}

#' Generate a sample of values from a vector and collapse them.
#' @description Generate a string summary of a vector by picking samples.
#' @param x A vector of values.
#' @param unique_only A logical value indicating whether to return unique values only.
#' @param n_samples The number of samples to return.
#' @param collapse The separator to use for collapsing the values.
#'
#' @returns A character string.
#' @export
#' @examples
#' get_samples(c(1, 2, 3, 4, 5))
#' get_samples(c(1, 2, 3, 4, 5), n_samples = 2)
#' get_samples(c(1, 2, 3, 3, 3), n_samples = 2, unique_only = TRUE)
#' get_samples(c(1, 2, 3, 4, 5), collapse = ", ")
get_samples <- function(x, unique_only = FALSE, n_samples = 10, collapse = "\n") {
  x <- na.omit(x)
  if (unique_only) {
    x <- unique(x)
  }
  y <- if (length(x) > n_samples) {
    sample(x, n_samples)
  } else {
    x
  }
  return(paste0(y, collapse = collapse))
}
