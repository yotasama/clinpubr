set.seed(1)
load_all()
library(knitr)
library(dtplyr)
library(clinpubr)
library(tictoc)

df <- data.frame(
  subject = c("A", "A", "A", "A", "B", "B", "B", "B", "B"),
  val = c("12.3", "45..6", "78a9", NA, "valid", "12.3a", "\uFF11\uFF12", "ok", "45..6")
)
df$unit=sample(letters,9)
get_samples <- function(x, n_samples=10, collapse = "_") {
  x <- na.omit(x)
  y <- if (length(x) > n_samples) {
    sample(x, n_samples)
  } else {
    x
  }
  return(paste0(y, collapse = collapse))
}
get_samples(df$val,n_samples = 3)
subject_view <- function(df, subject_col, info_cols, info_n_samples = 10, info_collapse = "_",
                         save_table = TRUE, filename = NULL) {
  res <- df %>%
    group_by(!!as.symbol(subject_col)) %>%
    summarise(
      target_subject = NA,
      count = n(),
      across(all_of(info_cols), ~get_samples(.x, n_samples = info_n_samples, collapse = info_collapse))
    )
  res <- as.data.frame(res)
  if (save_table) {
    if (is.null(filename)) {
      filename <- "subject_view.csv"
    }
    write.csv(res, file = filename, na = "")
  }
  return(res)
}
subject_view(df,subject_col = "subject", info_cols = c("val","unit"),info_collapse = "\n")
