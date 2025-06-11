set.seed(1)
load_all()
library(knitr)
library(dtplyr)
library(clinpubr)

df <- data.frame(
  subject = c("A", "A", "A", "A", "B", "B", "B", "B", "B"),
  val = c("12.3", "45..6", "78a9", NA, "valid", "12.3a", "\uFF11\uFF12", "ok", "45..6")
)
max_count = 20; random_sample = FALSE; long_df = T;
subject_col = NULL; value_col = NULL
library(dtplyr)
# Test default parameters (max_count=20, random_sample=FALSE)
result <- df_view_nonnum(lazy_dt(df), long_df = TRUE)
result <- df_view_nonnum(df, long_df = TRUE)
expect_s3_class(result, "data.frame")
# Include all non-numeric entries (valid, ok are non-numeric)
# Expect non-numeric entries followed by NAs (due to other columns having more entries)
expect_equal(result$A, c("45..6", "78a9", NA, NA, NA))  # NA is excluded
expect_equal(result$B, c("valid", "12.3a", "\uFF11\uFF12", "ok", "45..6"))

df_view_nonnum <- function(df, max_count = 20, random_sample = FALSE, long_df = FALSE,
                           subject_col = NULL, value_col = NULL) {
  if (ncol(df) == 0) {
    return(data.frame())
  }
  if (long_df && ncol(df) != 2 && (is.null(subject_col) || is.null(value_col))) {
    stop("`subject_col` and `value_col` must be specified for long `data.frame` with more than 2 columns.")
  }
  if (is.null(max_count) || max_count == 0) {
    max_count <- nrow(df)
  }
  if (long_df) {
    if (is.null(subject_col)) subject_col <- colnames(df)[1]
    if (is.null(value_col)) value_col <- colnames(df)[2]
    subjects <- df %>% dplyr::pull({{subject_col}}) %>% unique() %>% na.omit()
  } else {
    subjects <- colnames(df)
  }
  res <- data.frame(matrix(NA, nrow = max_count, ncol = length(subjects)))
  colnames(res) <- subjects
  for (subject in subjects) {
    df_sub <- if (long_df) {
      df %>%
        dplyr::filter((!!rlang::sym(subject_col)) == .esubject) %>%
        dplyr::pull(!!rlang::sym(value_col))
    } else {
      df %>% dplyr::pull(!!rlang::sym(subject))
    }
    x <- check_nonnum(df_sub)
    # Ensure strict max_count adherence
    if (length(x) > max_count) {
      x <- if (random_sample) sample(x, max_count) else x[1:max_count]
    }
    res[seq_along(x), subject] <- x
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