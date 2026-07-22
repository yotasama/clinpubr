# `split_multichoice()`

> **Split multi-choice data into columns**

## Description

Split multi-choice data into columns, each new column consists of
booleans whether a choice is presented.

## Usage

```r
split_multichoice(
  df,
  quest_cols,
  split = "",
  remove_space = TRUE,
  link = "_",
  remove_cols = TRUE
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `df` | A data frame. |
| `quest_cols` | A vector of column names that contain multi-choice data. |
| `split` | A string to split the data. Default is `""`. |
| `remove_space` | If `TRUE`, remove space in the data. |
| `link` | A string to link the column name and the option. Default is `"_"`. |
| `remove_cols` | If `TRUE`, remove the original columns. |

## Value

A data frame with additional columns.

## Examples

```r
df <- data.frame(q1 = c("ab", "c da", "b a", NA), q2 = c("a b", "a c", "d", "ab"))
split_multichoice(df, quest_cols = c("q1", "q2"))
```

