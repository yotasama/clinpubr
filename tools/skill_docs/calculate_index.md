# `calculate_index()`

> **Calculate index based on conditions**

## Description

Calculate an index based on multiple conditions. Each condition is evaluated and the
result is weighted and summed to produce the final index.

## Usage

```r
calculate_index(.df, ..., .weight = 1, .na_replace = 0)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `.df` | A data frame |
| `...` | Conditions to evaluate. See examples for more details. |
| `.weight` | Weight for each condition, should be of length 1 or equal to the number of conditions. |
| `.na_replace` | Value to replace `NA`, should be of length 1 or equal to the number of conditions. |

## Value

A numeric vector of index scores

## Examples

```r
df <- data.frame(x = c(1, 2, 3, 4, 5), y = c(1, 2, NA, 4, NA))
calculate_index(df, x > 3, y < 3, .weight = c(1, 2), .na_replace = 0)
```

