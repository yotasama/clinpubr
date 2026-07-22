# `subject_view()`

> **Get an overview of different subjects in data.**

## Description

Get a table of subject details for the clinical data.
This table could be labeled and used for subject name standardization.

## Usage

```r
subject_view(
  df,
  subject_col,
  info_cols,
  value_col = NULL,
  info_n_samples = 10,
  info_collapse = "\n",
  info_unique = FALSE,
  save_table = FALSE,
  filename = NULL
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `df` | A data frame of medical records that contains test subject, value, and unit cols. |
| `subject_col` | The name of the subject column. |
| `info_cols` | The names of the columns to get detailed information. |
| `value_col` | The name of the column that contains values. This column must be numerical. |
| `info_n_samples` | The number of samples to show in the detailed information columns. |
| `info_collapse` | The separator to use for collapsing the detailed information. |
| `info_unique` | A logical value indicating whether to show unique values only. |
| `save_table` | A logical value indicating whether to save the table to a csv file. |
| `filename` | The name of the csv file to be saved. |

## Value

A data frame of subject details.

## Examples

```r
df <- data.frame(subject = sample(c("a", "b"), 1000, replace = TRUE), value = runif(1000))
df$unit <- NA
df$unit[df$subject == "a"] <- sample(c("mg/L", "g/l", "g/L"),
  sum(df$subject == "a"),
  replace = TRUE
)
df$value[df$subject == "a" & df$unit == "mg/L"] <-
  df$value[df$subject == "a" & df$unit == "mg/L"] * 1000
df$unit[df$subject == "b"] <- sample(c(NA, "g", "mg"), sum(df$subject == "b"), replace = TRUE)
df$value[df$subject == "b" & df$unit %in% "mg"] <-
  df$value[df$subject == "b" & df$unit %in% "mg"] * 1000
df$value[df$subject == "b" & is.na(df$unit)] <- df$value[df$subject == "b" & is.na(df$unit)] *
  sample(c(1, 1000), size = sum(df$subject == "b" & is.na(df$unit)), replace = TRUE)
subject_view(
  df = df, subject_col = "subject", info_cols = c("value", "unit"), value_col = "value",
  save_table = FALSE
)
```

