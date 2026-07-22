# `combine_multichoice()`

> **Combine multi-choice columns into one**

## Description

Combine multi-choice columns into one, each column consists of
booleans whether a choice is presented.

## Usage

```r
combine_multichoice(
  df,
  quest_cols,
  sep = ",",
  remove_cols = TRUE,
  remove_prefix = TRUE
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `df` | A data frame. |
| `quest_cols` | A named list where each element is a character vector of column names to combine, or a single character vector. |
| `sep` | A string to separate the data. Default is `","`. |
| `remove_cols` | If `TRUE`, remove the original columns. |
| `remove_prefix` | If `TRUE`, automatically remove common prefix from column names when combining. |

## Value

A data frame with additional columns.

## Examples

```r
# Single group (backward compatibility)
df <- data.frame(q1 = c(TRUE, FALSE, TRUE), q2 = c(FALSE, TRUE, TRUE))
combine_multichoice(df, quest_cols = c("q1", "q2"))

# Multiple groups with named list
df <- data.frame(
  a1 = c(TRUE, FALSE, TRUE), a2 = c(FALSE, TRUE, TRUE),
  b1 = c(TRUE, TRUE, FALSE), b2 = c(FALSE, FALSE, TRUE)
)
combine_multichoice(df, quest_cols = list(groupA = c("a1", "a2"), groupB = c("b1", "b2")))
```

