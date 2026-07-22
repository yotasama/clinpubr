# `df_view_nonnum()`

> **Show non-numeric elements in a data frame**

## Description

Shows the non-numeric elements in a data frame. Only character columns are checked.
Useful when setting the strategy to clean numeric values.

## Usage

```r
df_view_nonnum(
  df,
  max_count = 20,
  random_sample = FALSE,
  long_df = FALSE,
  subject_col = NULL,
  value_col = NULL
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `df` | A data frame. |
| `max_count` | An integer. The maximum number of elements to show for each column. If `NULL` or `0`, show all elements, not recommended due to huge memory waste. |
| `random_sample` | A logical value. If `TRUE`, randomly sample the elements to show. |
| `long_df` | A logical value. If `TRUE`, the input `df` is provided in a long format. |
| `subject_col` | A character string. The name of the column that contains the subject identifier. Used when `long_df` is `TRUE`. If `NULL`, the subject column is assumed to be the first column. |
| `value_col` | A character string. The name of the column that contains the values. Used when `long_df` is `TRUE`. If `NULL`, the value column is assumed to be the second column. |

## Value

A data frame of the non-numeric elements.

## Examples

```r
df <- data.frame(
  x = c("1", "2", "3..3", "4", "6a"),
  y = c("1", "ss", "aa.a", "4", "xx"),
  z = c("1", "2", "3", "4", "6")
)
df_view_nonnum(df)
```

