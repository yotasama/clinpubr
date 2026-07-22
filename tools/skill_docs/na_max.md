# `na_max()`

> **Safe min and max functions that return NA if all values are NA**

**别名**: `na_min()`

## Description

Instead of returning `-Inf` or `Inf`, returns `NA` if all values are `NA`.
It also ignores `NA` values by default, which is different from base R functions.
This is useful when summarizing data frames with `dplyr::summarise()`.

## Usage

```r
na_max(x, na.rm = TRUE)

na_min(x, na.rm = TRUE)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `x` | A numeric vector. |
| `na.rm` | A logical value indicating whether to remove `NA` values before computation. Defaults to `TRUE` instead of `FALSE` in base R functions. |

## Value

The minimum or maximum value of the vector or `NA` if all values are `NA`.

## Examples

```r
na_max(c(1, 2, 3, NA))
na_min(c(NA, NA, NA))
```

