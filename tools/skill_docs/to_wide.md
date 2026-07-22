# `to_wide()`

> **Fast long-to-wide conversion with item selection**

## Description

Convert long-format data to wide format by grouping keys,
using one column as item names and one column as values. When there are
multiple values under the same key-item combination, values are reduced
by `agg_fun`. Designed to convert long-format clinical data in database
to wide format for analysis and publication.

## Usage

```r
to_wide(df, keys, item_col, value_col, items = NULL, agg_fun = get_valid)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `df` | A data frame in long format. |
| `keys` | A character vector of key column names. |
| `item_col` | A single column name that contains item names. |
| `value_col` | A single column name that contains values to spread. |
| `items` | Optional character vector of items to keep in wide format. If provided, output item columns follow this order and missing items are kept as `NA` columns. |
| `agg_fun` | Aggregation function used when key-item combinations have multiple values. Default is `get_valid()`. |

## Value

A wide-format data frame.

## Examples

```r
df <- data.frame(
  id = c(1, 1, 1, 2, 2),
  visit = c("v1", "v1", "v1", "v1", "v1"),
  item = c("A", "A", "B", "A", "C"),
  value = c(3, 5, 2, 1, 9)
)

to_wide(
  df,
  keys = c("id", "visit"),
  item_col = "item",
  value_col = "value",
  items = c("A", "B", "C"),
  agg_fun = max
)
```

