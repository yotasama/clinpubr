# `get_valid()`

> **Get one valid value from vector.**

## Description

Extract one valid (non-NA) value from a vector.

## Usage

```r
get_valid(x, mode = c("first", "mid", "last"), disjoint = FALSE)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `x` | A vector. |
| `mode` | The mode of the valid value to extract. `"first"` extracts the first valid value, `"last"` extracts the last valid value, and `"mid"` extracts the middle valid value. |
| `disjoint` | If TRUE, the values extracted by the three modes are forced to be different. This behavior might be desired when trying to extract different values with different modes. The three modes extract values in the sequence: "first", "last", "mid". |

## Value

A single valid value from the vector. `NA` if all values are invalid.

## Examples

```r
get_valid(c(NA, 1, 2, NA, 3, NA, 4))
get_valid(c(NA, 1, NA), mode = "last", disjoint = TRUE)
```

